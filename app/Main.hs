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

-- RELEASE: 0.5.0. Add `add --library pdfFolder library.bib`

data AddSource
  = FromRaw FilePath
  | FromDOI DOI
  deriving Show

data Filter
  = ByAuthor  Text
  | ByProject Text
  | ByQuery   [Text]
  deriving Show

-- papers get --project masters
-- papers get --author einstein
-- papers list --project masters
-- papers list --author einstein

data ExtractFilter
  = ExtractFilter Filter
  | ExtractAll
  deriving Show

data Context
  = List (Maybe Filter)
  | Open Text
  | Add (Maybe FilePath) AddSource -- (pdf, bib, projects)
  | Extract ExtractFilter
  | Edit
  | Attatch Text AddSource
  | Info Text
  | Rename Text Text
  | Remove Bool Text
  deriving Show

optionParser :: Parser Context
optionParser =
  hsubparser
    ( command  "extract" (info extractParser (progDesc "Extract entries and cat to stdout"))
    <> command "get"     (info extractParser (progDesc "Extract entries and cat to stdout -- alias to extract"))
    <> command "add"     (info addParser     (progDesc "Add a pdf-bibtex pair"))
    <> command "open"    (info openParser    (progDesc "Open a single entry in `$PDF_VIEWER`"))
    <> command "list"    (info listParser    (progDesc "List all entries"))
    <> command "attatch" (info attatchParser (progDesc "Attatch a pdf to an existing entry"))
    <> command "ls"      (info listParser    (progDesc "List all entries -- alias for list"))
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
removeParser = Remove <$>
  switch ( long "force" <> help "Forcefully remove the item" )
  <*> (T.pack <$> strArgument ( metavar "ITEM" <> help "Key/query to remove" ))

extractParserFilter :: Parser ExtractFilter
extractParserFilter = flag' ExtractAll ( long "all" <> help "Extract all entries" )
                      <|> ExtractFilter <$> entryFilterParser

entryFilterParser :: Parser Filter
entryFilterParser = ByProject <$>
  option str ( long "project" <> metavar "PROJECT" <> help "Filter by project")
  <|> ByAuthor <$> option str
        ( long "author" <> metavar "AUTHOR" <> help "Filter by author substring" )
  <|> ByQuery <$> some (strArgument
        ( metavar "ITEMS..." <> help "achs_jacsat118_9360.bibKeys/queries to match" ))

infoParser' :: Parser Context
infoParser' = Info <$> strArgument ( metavar "Item" <> help "Query to info" )

renameParser :: Parser Context
renameParser = Rename <$>
  strArgument ( metavar "Item" <> help "Query to rename" )
  <*> strArgument ( metavar "Item" <> help "New key" )

attatchParser :: Parser Context
attatchParser = Attatch <$>
  strArgument ( metavar "Item"  <> help "Query to attatch to" )
  <*> (
      FromRaw <$> strArgument (metavar "PDF" <> help "PDF source file")
  <|> FromDOI <$> option str (long "doi" <> metavar "DOI" <> help "Fetch PDF using DOI if possible"))

editParser :: Parser Context
editParser = pure Edit

addParser :: Parser Context
addParser = onlyBibMode <|> pdfMode
  where
    onlyBibMode :: Parser Context
    onlyBibMode =
      Add Nothing <$ onlyBibFlag <*> addSourceParser

    pdfMode :: Parser Context
    pdfMode =
      (Add . Just <$> strArgument ( metavar "PDF" <> help "PDF source file" ))
        <*> addSourceParser

    onlyBibFlag :: Parser ()
    onlyBibFlag =
      flag' ()
        ( long "only-bib"
       <> short 'b'
       <> help "Add an entry with no PDF (BibTeX required via FILE or --doi)"
        )

addSourceParser :: Parser AddSource
addSourceParser =
      FromRaw <$> strArgument (metavar "BIB" <> help "BibTeX source file")
  <|> FromDOI <$> option str (long "doi" <> metavar "DOI" <> help "Fetch BibTeX from DOI")

openParser :: Parser Context
openParser = Open <$> strArgument ( metavar "Item" <> help "Entry to open" )

listParser :: Parser Context
listParser = List <$> optional entryFilterParser

ctxInfo :: ParserInfo Context
ctxInfo =
  info (optionParser <**> helper)
    ( fullDesc
    <> progDesc "A cli reference manager that fits the unix philosophy."
    <> header "papers - A cli reference manger."
    )

data Entry = Entry
  { key      :: Text
  , pdfPath  :: Maybe FilePath
  , bibPath  :: FilePath
  , authors  :: Text
  , title    :: Text
  , keywords :: [Text]
  , projects :: [Text]
  } deriving (Show)

instance Eq Entry where
  (==) e1 e2 = e1.key == e2.key
                -- || e1.pdfPath == e2.pdfPath
                -- || e1.title == e2.title

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
  <$> Toml.text "key"                     .= key
  <*> Toml.dioptional (Toml.string "pdf") .= pdfPath
  <*> Toml.string "bib"                   .= bibPath
  <*> Toml.text "authors"                 .= authors
  <*> Toml.text "title"                   .= title
  <*> Toml.arrayOf Toml._Text "keywords"  .= keywords
  <*> Toml.arrayOf Toml._Text "projects"  .= projects

entriesCodec :: TomlCodec [Entry]
entriesCodec = Toml.list entryCodec "entry"

-- dupes :: Eq a => [a] -> Either [a] [a]
-- dupes xs = let xs' = nub xs
--           in if not (null $ xs \\ xs')
--             then Left [] -- add the inverse-intersection
--             else Right xs

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
  es  <- case res of
    Left err  -> fail (show err)
    Right es  -> pure es
  case es of
    (e:_) -> pure e
    []    -> die $ "No BibTex entries in " <> fp

extractBib :: FilePath -> IO (Text, Text, Text)
extractBib fp = do
  et <- parseBib fp
  let key'   = et.identifier
      title  = getField "title" et
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

-- Rewrite since we do the majority of the same things.
createEntry :: [Entry] -> FilePath -> Maybe FilePath -> AddSource -> IO Entry
createEntry es base pdfSrc bib = do
  -- ensure pdfSrc
  bibSrc <- case bib of
    FromRaw r   -> pure r
    FromDOI doi -> do
      bibEntry <- fetchBibFromDoi doi
      TIO.writeFile "temp_file.bib" bibEntry
      pure "temp_file.bib"
  (key', title, author) <- extractBib bibSrc
  when (key' `elem` map key es) $ die
    $ T.unpack $ "Key already exists: " <> key'
    <> ". Consider renaming the key in `temp_file.bib`."
  (pdfdest, bibdest) <- copyIntoLibrary base pdfSrc bibSrc key'
  case bib of
    FromDOI _ -> removeFile bibSrc
    _         -> pure ()
  pure Entry
      { key = key'
      , pdfPath = pdfdest
      , bibPath = bibdest
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
    [e] -> maybe (die $ T.unpack $ "No affilieted pdf with" <> e.key) openPdf e.pdfPath
    xs  -> die $ T.unpack $ "Multiple matches for: " <> query
                  <> "\nMatches: " <> T.intercalate ", " (map key xs)

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

filterEntries :: [Entry] -> Maybe Filter -> [Entry]
filterEntries es filter'
  = case filter' of
      Nothing -> es
      Just f  -> case f of
        ByQuery qs  -> nub $ concatMap (matchEntries es) qs
        ByProject p ->
            filter (\e -> T.toCaseFold p `elem` map T.toCaseFold e.projects) es
        ByAuthor a  ->
            filter (\e -> T.toCaseFold a `T.isInfixOf` T.toCaseFold e.authors) es

listEntry :: [Entry] -> Maybe Filter -> IO ()
listEntry es filter' = do
  let maxKey = maximum (1 : [T.length e.key | e <- es]) + 1
      es'    = sortOn (T.toCaseFold . key) (filterEntries es filter')
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
        ExtractFilter query' -> filterEntries es (Just query')
        ExtractAll    -> es
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

  -- Should we really die here? Or should we info many entries?
  match <- case matches of
        [s] -> pure s
        []  -> die $ T.unpack $ "No matches for" <> query
        xs  -> die $ T.unpack $ "Multiple results for: " <> query
                    <> "\nMatches: " <> T.intercalate ", " (map key xs)

  let fp = bibDest base match.key
  bib <- parseBib fp

  let title    = getInfo "title" bib
      author   = getInfo "author" bib
      abstract = getField "abstract" bib
      -- abstract = getInfo "abstract" bib -- getField "abstract" bib
      fields   =  ["Type: " <> T.pack bib.entryType
                  , "Title: " <> title
                  , "Author: " <> author
                  , T.pack $ "Abstract: " ++ fromMaybe "" abstract]

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
  where
    getInfo field bib = trim $ T.pack $ fromMaybe "Uknown" (getField field bib)

editEntry :: FilePath -> IO ()
editEntry base = do
  editor <- lookupEnv "EDITOR"
  let fp = base </> "meta.toml"
      cmd = maybe "vi" id editor
  ensure fp
  _ <- callCommand $ cmd ++ " " ++ fp
  pure ()

copyIntoLibrary :: FilePath -> Maybe FilePath -> FilePath -> Text -> IO (Maybe FilePath, FilePath)
copyIntoLibrary base pdfSrc bibSrc k = do
  createDirectoryIfMissing True (pdfDir base)
  createDirectoryIfMissing True (bibDir base)

  let bibOut = bibDest base k
      pdfOut = pdfDest base k

  copyFile bibSrc bibOut

  case pdfSrc of
    Nothing   -> pure (Nothing, bibOut)
    Just p    -> do
      copyFile p pdfOut
      pure (Just pdfOut, bibOut)

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
    Add pdfpath bibpath -> do
      entry <- createEntry stmts base pdfpath bibpath
      writeToToml base (stmts ++ [entry])
      TIO.putStrLn $ "Added `" <> entry.key <> "` to library!"
    Attatch query pdf -> do
      (entry, stmts') <- case findEntryPair stmts query "__UNUSED__KEY__" of
        Left msg -> die $ T.unpack msg
        Right r  -> pure r

      entry' <- case pdf of
        FromRaw fp  -> pure $ entry{pdfPath = Just fp}
        FromDOI _   -> die "Not implemented yet"

      putStrLn "hello"
      writeToToml base (entry':stmts')
    
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
        writeToToml base stmts'

    Rename query nkey -> do

      (entry, stmts') <- case findEntryPair stmts query nkey of
                              Left msg -> die $ T.unpack msg
                              Right r  -> pure r
      -- FIX:DONE: Right now we dont change the entry of the bibfile itself
      -- and thus the extracted entry does contain the old key.
      -- Below is a possible fix.
      -- This should work
      bib <- parseBib entry.bibPath
      let bib' = bib { Bib.identifier = T.unpack nkey }
      TIO.writeFile entry.bibPath (T.pack $ BIBFormat.entry bib')

      (pdfPath, bibPath) <- copyIntoLibrary base
                        entry.pdfPath entry.bibPath nkey

      let entry' = entry
                    { key = nkey
                    , pdfPath = pdfPath
                    , bibPath = bibPath}

      writeToToml base (stmts' ++ [entry'])
      TIO.putStrLn $ "Renamed `" <> query <> "` to `"
                      <> nkey <> "` in the library!"
    where
      newToml = Toml.encode entriesCodec
      writeToToml base st = TIO.writeFile (base </> "meta.toml") (newToml st)

