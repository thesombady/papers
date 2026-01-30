{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.List ( nub, (\\), sortOn )
import Data.Maybe ( fromMaybe )

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Toml ( TomlCodec, (.=) )
import qualified Toml

import System.Exit (die)
import System.Info ( os )
import System.FilePath ( (</>) )
import System.Environment ( lookupEnv )
import System.Directory
  ( doesFileExist
  , getHomeDirectory
  , renameFile
  , copyFile
  , createDirectoryIfMissing
  , removeFile)
import System.Process
  ( spawnProcess
  , callCommand
  , readCreateProcess
  , shell)

import Control.Monad ( when )
import Options.Applicative

import Text.Parsec.String (parseFromFile)
import qualified Text.BibTeX.Parse as BibParse
import qualified Text.BibTeX.Entry as Bib
import qualified Text.BibTeX.Format as BIBFormat

type DOI = Text

data AddSource
  = FromBib FilePath
  | FromDOI DOI
  deriving Show

data ListFilter
  = AuthorFilter Text
  | ProjectFilter Text
  | NoFilter
  deriving Show

data ExtractFilter
  = ExtractProject Text
  | ExtractEntries [Text]
  | ExtractAll
  deriving Show

data Context
  = List ListFilter
  | Open Text
  | Add FilePath AddSource -- (pdf, bib, projects)
  | Extract ExtractFilter
  | Edit
  | Info Text
  | Rename Text Text
  | Remove Bool Text
  -- Edit Text (open meta.toml at the line number in $EDITOR)
  deriving Show

optionParser :: Parser Context
optionParser =
  hsubparser
    ( command  "extract" (info extractParser (progDesc "Extract entries and cat to stdout"))
    <> command "get"     (info extractParser (progDesc "Extract entries and cat to stdout -- alias to extract"))
    <> command "add"     (info addParser     (progDesc "Add a pdf-bibtex pair"))
    <> command "open"    (info openParser    (progDesc "Open a single entry in `$PDF_VIEWER`"))
    <> command "list"    (info listParser    (progDesc "List all entries"))
    <> command "edit"    (info editParser    (progDesc "Edit the metafile in $EDITOR or vi"))
    <> command "info"    (info infoParser'   (progDesc "Obtain information about the query result"))
    <> command "rename"  (info renameParser  (progDesc "Rename an entry"))
    <> command "mv"      (info renameParser  (progDesc "Rename (move) an entry -- alias for rename"))
    <> command "remove"  (info removeParser  (progDesc "Remove an entry"))
    <> command "rm"      (info removeParser  (progDesc "Remove an entry -- alias for remove"))
    )

extractParser :: Parser Context
extractParser = Extract <$> extractParserFilter

removeParser :: Parser Context
removeParser =
  Remove
    <$> switch
        ( long "force"
       <> help "Forcefully remove the item"
        )
    <*> (T.pack <$> strArgument
        ( metavar "ITEM"
       <> help "Key/query to remove"
        ))

extractParserFilter :: Parser ExtractFilter
extractParserFilter =
  flag' ExtractAll
    ( long "all"
    <> help "Extract all entries"
    )
  <|> ExtractProject <$> option str
    ( long "project"
    <> metavar "Project"
    <> help "Specific Project to list"
    )
  <|> ExtractEntries <$> some (strArgument
    ( metavar "Items..."
    <> help "Entries to extract"
    ))

infoParser' :: Parser Context
infoParser' =
  Info <$> strArgument
    ( metavar "Item"
   <> help "Query to info"
    )

renameParser :: Parser Context
renameParser =
  Rename <$> strArgument
    ( metavar "Item"
   <> help "Query to rename"
    )
  <*> strArgument
    (metavar "Item"
    <> help "New key"
    )

editParser :: Parser Context
editParser = pure Edit

addParser :: Parser Context
addParser =
  Add
    <$> strArgument
        ( metavar "Source"
       <> help "Pdf source file"
        )
    <*> addSourceParser

addSourceParser :: Parser AddSource
addSourceParser =
      FromBib <$> strArgument
        ( metavar "BIB"
       <> help "BibTeX source file"
        )
  <|> FromDOI <$> option str
        ( long "doi"
       <> metavar "DOI"
       <> help "Fetch BibTeX from DOI"
        )

openParser :: Parser Context
openParser =
  Open <$> strArgument
    ( metavar "Item"
    <> help "Entry to open"
    )

listParser :: Parser Context
listParser = List <$> listParserFilter
              <|> pure (List NoFilter)

listParserFilter :: Parser ListFilter
listParserFilter =
  AuthorFilter <$> option str
    (long "author"
    <> metavar "Author"
    <> help "Specific author to list"
    )
  <|> ProjectFilter <$> option str
    (long "project"
    <> metavar "Project"
    <> help "Specify project to list"
    )

ctxInfo :: ParserInfo Context
ctxInfo =
  info (optionParser <**> helper)
    ( fullDesc
    <> progDesc "A cli reference manager that fits the unix philosophy."
    <> header "papers - A cli reference manger."
    )

data Entry = Entry
  { key      :: Text
  , pdfPath  :: Text
  , bibPath  :: Text
  , authors  :: Text
  , title    :: Text
  , keywords :: [Text]
  , projects :: [Text]
  } deriving (Show)

instance Eq Entry where
  (==) e1 e2 = e1.key == e2.key
                || e1.pdfPath == e2.pdfPath
                || e1.title == e2.title

matchEntry :: Text -> Entry -> Bool
matchEntry query e
  | query == T.toCaseFold e.key = True
  | prefix' e.key               = True
  | any prefix' e.keywords      = True
  | infix' e.authors            = True
  | infix' e.title              = True
  | otherwise                   = False
  where
    prefix' q = query `T.isPrefixOf` T.toCaseFold q
    infix'  q = query `T.isInfixOf` T.toCaseFold q

matchEntries :: [Entry] -> Text -> [Entry]
matchEntries es query = filter (matchEntry
                      ((T.toCaseFold . T.strip) query)) es

entryCodec :: TomlCodec Entry
entryCodec = Entry
  <$> Toml.text "key"      .= key
  <*> Toml.text "pdf"      .= pdfPath
  <*> Toml.text "bib"      .= bibPath
  <*> Toml.text "authors"  .= authors
  <*> Toml.text "title"    .= title
  <*> Toml.arrayOf Toml._Text "keywords" .= keywords
  <*> Toml.arrayOf Toml._Text "projects" .= projects

data Entries where
  Entries :: {entries :: ![Entry]} -> Entries
  deriving (Show, Eq)

entriesCodec :: TomlCodec [Entry]
entriesCodec = Toml.list entryCodec "entry"

dupes :: Eq a => [a] -> Either [a] [a]
dupes xs = let xs' = nub xs
          in if not (null $ xs \\ xs')
            then Left [] -- add the inverse-intersection
            else Right xs

ensure :: FilePath -> IO ()
ensure f = do ok <- doesFileExist f
              if ok then pure ()
              else die  $ "File `" ++ f ++ "` does not exist"

type BibEntry = Bib.T

getField :: String -> BibEntry -> Maybe String
getField query e = lookup query e.fields

parseBib :: FilePath -> IO BibEntry
parseBib fp = do
  res <- parseFromFile BibParse.file fp
  es <- case res of
    Left err  -> fail (show err)
    Right es  -> pure es
  case es of
    (e:_) -> pure e
    []    -> die $ "No BibTex entries in " <> fp

extractBib :: FilePath -> IO (Text, Text, Text)
extractBib fp = do
  et <- parseBib fp
  let key' = et.identifier
      title = getField "title" et
      author = getField "author" et

  (title', author') <- case (title, author) of
      (Just t, Just a) -> pure (t, a)
      (_, _)           -> die "Missing field/s (title or author)"

  pure  (T.pack key'
        , trim (T.pack title')
        , trim (T.pack author'))

fetchBibFromDoi :: DOI -> IO Text
fetchBibFromDoi doi = do
  let cmd = "curl -fsSL -H 'Accept: application/x-bibtex' https://doi.org/"
              <> T.unpack doi
  out <- readCreateProcess (shell cmd) ""
  if null out
    then die $ "FAILED to fetch BibTex from DOI: " <> T.unpack doi
    else pure (T.pack out)

moveIntoLibrary :: FilePath -> FilePath -> FilePath -> Text -> IO (FilePath, FilePath)
moveIntoLibrary base pdfSrc bibSrc key = do
  ensure pdfSrc
  ensure bibSrc

  let pdfPath = pdfDest base key
      bibPath = bibDest base key

  renameFile pdfSrc pdfPath
  renameFile bibSrc bibPath
  pure (pdfPath, bibPath)

createEntry :: FilePath -> FilePath -> AddSource -> IO Entry
createEntry base pdfsrc (FromBib bibsrc) = do
  ensure pdfsrc
  (key, title, author) <- extractBib bibsrc
  -- TODO CRITITAL: Make sure that no entry already exists with the key.
  (pdfdest, bibdest) <- copyIntoLibrary base pdfsrc bibsrc key
  pure Entry
    { key = key
    , pdfPath = T.pack pdfdest
    , bibPath = T.pack bibdest
    , authors = author
    , title = title
    , keywords = []
    , projects = []
    }
createEntry base pdfsrc (FromDOI doi) = do
  ensure pdfsrc
  bibsrc <- fetchBibFromDoi doi
  -- TODO CRITICAL: Make sure that no entry already exists with the key.
  TIO.writeFile "temp_file.bib" bibsrc
  (key, title, author) <- extractBib "temp_file.bib"
  TIO.putStrLn $ "Generated key: " <>  key
  (pdfdest, bibdest) <- copyIntoLibrary base pdfsrc "temp_file.bib" key
  removeFile "temp_file.bib"
  pure Entry
    { key = key
    , pdfPath = T.pack pdfdest
    , bibPath = T.pack bibdest
    , authors = author
    , title = title
    , keywords = []
    , projects = []
    }

openCmd :: String
openCmd
  | os == "darwin" = "open"
  | otherwise      = "xdg-open"

openPdf :: FilePath -> IO ()
openPdf fp = do
  ensure fp
  viewer <- lookupEnv "PDF_VIEWER"
  let cmd = maybe openCmd id viewer
  _ <- spawnProcess cmd [fp]
  pure ()

openEntry :: [Entry] -> Text -> IO ()
openEntry es query = do
  let matches = nub $ matchEntries es query
  case matches of
    []  -> die $ "No matches for: " <> T.unpack query
    [e] -> openPdf (T.unpack e.pdfPath)
    _   -> die $ "Multiple matches for: " <> T.unpack query
  -- if length matches > 1 then error $ "Multiply matches for: " ++ T.unpack query
  -- else openPdf (T.unpack $ pdfPath (head matches))

truncateText :: Int -> Text -> Text
truncateText n s
  | n <= 1         = T.take n s
  | T.length s <= n  = s
  | otherwise      = T.take (n - 1) s <> "…"

padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (max 0 (n - T.length t)) " "

formatRow :: Int -> Int -> Int -> Int -> Entry -> Text
formatRow keyW titleW projectW keywordsW e =
  padRight keyW (truncateText keyW e.key)         <> "  " <>
  padRight titleW (truncateText titleW e.title)   <> "  " <>
  padRight projectW  (truncateText projectW proj) <> "  " <>
  padRight keywordsW keys
  where
    proj = if null e.projects then "    "
           else T.pack "[" <> truncateText (projectW - 2) (T.intercalate ", " e.projects) <> "]"
    keys = if null e.keywords then ""
           else T.pack "[" <> truncateText keywordsW (T.intercalate ", " e.keywords) <> "]"

filterEntries :: [Entry] -> ListFilter -> [Entry]
filterEntries es filter'
  = case filter' of
      NoFilter -> es
      ProjectFilter query ->
        filter (\e -> T.toCaseFold query `elem` map T.toCaseFold e.projects) es
      AuthorFilter query  ->
        filter (\e -> T.toCaseFold query `T.isInfixOf` T.toCaseFold e.authors) es

listEntry :: [Entry] -> ListFilter -> IO ()
listEntry es filter' = do
  let maxKey = maximum (1 : [T.length e.key | e <- es]) + 1
      es'    = sortOn key (filterEntries es filter')
      rows   = map (formatRow maxKey 60 15 15) es'
  TIO.putStrLn "  Reference list  "
  TIO.putStrLn $
    padRight (maxKey + 2) "Key"
    <> padRight 62 "Title"
    <> padRight 17 "Projects"
    <> padRight 1 "Keywords"
  TIO.putStrLn $ T.replicate (maxKey + 60 + 15 * 2 + 8) "="
  TIO.putStrLn $ T.intercalate "\n" rows

pdfDir :: FilePath -> FilePath
pdfDir base = base </> "pdfs"

bibDir :: FilePath -> FilePath
bibDir base = base </> "bibs"

pdfDest :: FilePath -> Text -> FilePath
pdfDest base key = pdfDir base </> T.unpack key <> ".pdf"

bibDest :: FilePath -> Text -> FilePath
bibDest base key = bibDir base </> T.unpack key <> ".bib"

extractEntry :: [Entry] -> ExtractFilter -> IO ()
extractEntry es query = do
  home <- getHomeDirectory
  let base = home </> ".Papers/"
  let matches = case query of
          ExtractEntries queries -> nub $ concatMap (matchEntries es) queries
          ExtractAll             -> es
          ExtractProject query'  -> filterEntries es (ProjectFilter query')
      files   = [ bibDest base entry.key | entry <- matches]
  cats <- mapM TIO.readFile files
  if not (null cats) then do
    TIO.putStrLn $ T.intercalate "\n\n" cats
  else
    die "No entries found."

trim :: Text -> Text
trim = T.unwords . T.words . T.replace "\n\t" ""

infoEntry :: [Entry] -> Text -> IO ()
infoEntry es query = do
  home <- getHomeDirectory
  let base = home </> ".Papers/"
  let matches = nub $ matchEntries es query

  match <- case matches of
        [s] -> pure s
        _   -> die $ "Multiple results for: " ++ T.unpack query

  let fp = bibDest base match.key
  bib <- parseBib fp

  let title  = trim $ T.pack $
                  fromMaybe "Unknown" (getField "title" bib)
      author = trim $ T.pack $
                  fromMaybe "Unknown" (getField "author" bib)
      abstract = getField "abstract" bib

  TIO.putStrLn $ "Type: " <> T.pack bib.entryType
  case match.projects of
    (_:_) -> TIO.putStrLn $ "Projects: " <> T.intercalate ", " match.projects
    []    -> pure ()

  case match.keywords of
    (_:_) -> TIO.putStrLn $ "Keywords: " <> T.intercalate ", " match.keywords
    []    -> pure ()

  TIO.putStrLn $ "Author: " <> author
  TIO.putStrLn $ "Title: "  <> title
  case abstract of
    Nothing -> pure ()
    Just abs'  -> do
      TIO.putStrLn $ "Abstract: "
        <> T.intercalate ".\n" (T.splitOn ". " (trim (T.pack abs')))

editEntry :: FilePath -> IO ()
editEntry base = do
  editor <- lookupEnv "EDITOR"
  let fp = base </> "meta.toml"
      cmd = maybe "vi" id editor
  ensure fp
  _ <- callCommand $ cmd ++ " " ++ fp
  pure ()

copyIntoLibrary :: FilePath -> FilePath -> FilePath -> Text -> IO (FilePath, FilePath)
copyIntoLibrary base pdfSrc bibSrc k = do
  createDirectoryIfMissing True (pdfDir base)
  createDirectoryIfMissing True (bibDir base)

  let pdfOut = pdfDest base k
      bibOut = bibDest base k

  copyFile pdfSrc pdfOut
  copyFile bibSrc bibOut

  pure (pdfOut, bibOut)

findEntryPair :: [Entry] -> Text -> Text -> Either Text (Entry, [Entry])
findEntryPair es query nkey =
  let matches   = nub $ matchEntries es query
      contained = any (\x -> x.key == nkey) es
  in case (matches, contained) of
    ([], False)      -> Left "No match"
    ([], True)       -> Left "No match, but key already exists"
    ([match], False) -> Right (match, filter (/= match) es)
    (_, False)       -> Left $ "Multiple matches for `" <> query <>"`: "
                        <> T.intercalate ", " (map key matches)
    (_, True)        -> Left $ "Key " <> nkey
                        <> " already exists, and found multiple matches for `"
                        <> query <>"`: "
                        <> T.intercalate ", " (map key matches)

main :: IO ()
main = do
  home <- getHomeDirectory
  let base = home </> ".Papers"
  ensure $ base </> "meta.toml"
  input <- TIO.readFile $ base </> "meta.toml"
  stmts <- case Toml.decode entriesCodec input of
                Left msgs     -> die (T.unpack $ Toml.prettyTomlDecodeErrors msgs)
                Right entries -> pure entries
  ctx <- execParser ctxInfo

  case ctx of
    List filter'      -> listEntry stmts filter'
    Open query        -> openEntry stmts query
    Extract query     -> extractEntry stmts query
    Edit              -> editEntry base
    Info query        -> infoEntry stmts query
    Remove mode query -> do
      (entry', stmts') <- case findEntryPair stmts query query of
        Left msg -> die $ T.unpack msg
        Right r   -> pure r

      remove' <- (if mode then (do
        pure True) else (do
        TIO.putStrLn $ "Do you want to remove " <> entry'.key <> "? y/N"
        response <- getChar
        if response `elem` ['n', 'N'] then (do
          TIO.putStrLn $ entry'.key <> " is not removed from the library."
          pure False)
        else pure True))

      when remove' $ do
        removeFile (pdfDest base entry'.key)
        removeFile (bibDest base entry'.key)
        TIO.writeFile (base </> "meta.toml") (Toml.encode entriesCodec stmts')

    Rename query nkey -> do

      (entry, stmts') <- case findEntryPair stmts query nkey of
                              Left msg -> die $ T.unpack msg
                              Right r  -> pure r

      -- FIX:DONE: Right now we dont change the entry of the bibfile itself
      -- and thus the extracted entry does contain the old key.
      -- Below is a possible fix.
      -- This should work

      bib <- parseBib (T.unpack entry.bibPath)

      let bib' = bib { Bib.identifier = T.unpack nkey }

      TIO.writeFile (T.unpack entry.bibPath) (T.pack $ BIBFormat.entry bib')

      (pdfPath, bibPath) <- copyIntoLibrary base
                        (T.unpack entry.pdfPath) (T.unpack entry.bibPath) nkey
       
      let entry' = entry
                    { key = nkey
                    , pdfPath = T.pack pdfPath
                    , bibPath = T.pack bibPath}

      writeToToml base stmts' entry'
      TIO.putStrLn $ "Renamed `" <> query <> "` to `"
                      <> nkey <> "` in the library!"
    Add pdfpath bibpath -> do
      entry <- createEntry base pdfpath bibpath
      let contains' = entry `elem` stmts
      if contains'
        then die "Entry already exists."
        else do
            -- let newStmts = stmts ++ [entry]
            --     newToml  = Toml.encode entriesCodec newStmts
            -- TIO.writeFile (base </> "meta.toml") newToml
            writeToToml base stmts entry
            TIO.putStrLn $ "Added `" <> entry.key <> "` to library!"
    where
      newStmts st et = st ++ [et]
      newToml st et = Toml.encode entriesCodec (newStmts st et)
      writeToToml base st et = TIO.writeFile (base </> "meta.toml") (newToml st et)

