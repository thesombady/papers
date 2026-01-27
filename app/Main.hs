{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.List ( nub, (\\), sortOn )
import System.Directory
  ( doesFileExist
  , getHomeDirectory
  , copyFile
  , createDirectoryIfMissing
  , removeFile)
import System.Exit (die)
import System.FilePath ( (</>) )
import Toml ( TomlCodec, (.=) )
import qualified Toml
import Data.Text ( Text )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (spawnProcess, callCommand, readCreateProcess, shell)
import System.Environment (lookupEnv)
import System.Info ( os )
import Options.Applicative
import Text.Parsec.String (parseFromFile)
import qualified Text.BibTeX.Parse as BibParse
import qualified Text.BibTeX.Entry as Bib

type DOI = Text

data AddSource
  = FromBib FilePath
  | FromDOI DOI
  deriving Show

data Context
  = List
  | Open Text
  | Add FilePath AddSource -- (pdf, bib, projects)
  | Extract [Text]
  | Edit
  | Info Text
  -- Edit Text (open meta.toml at the line number in $EDITOR)
  deriving Show

optionParser :: Parser Context
optionParser =
  hsubparser
    ( command "extract" (info extractParser (progDesc "Extract entries and cat to stdout"))
   <> command "add"     (info addParser     (progDesc "Add a pdf-bibtex pair"))
   <> command "open"    (info openParser    (progDesc "Open a single entry in `$PDF_VIEWER`"))
   <> command "list"    (info listParser    (progDesc "List all entries"))
   <> command "edit"    (info editParser    (progDesc "Edit the metafile in $EDITOR or vi"))
   <> command "info"    (info infoParser'   (progDesc "Obtain information about the query result"))
    )

extractParser :: Parser Context
extractParser =
  Extract <$> some (strArgument
    ( metavar "Items..."
   <> help "Entries to extract"
    ))

infoParser' :: Parser Context
infoParser' =
  Info <$> strArgument
    ( metavar "Item"
   <> help "Query to info"
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
listParser = pure List

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

matchEntries :: Text -> [Entry] -> [Entry]
matchEntries query = filter (matchEntry
                      ((T.toCaseFold . T.strip) query))

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

createEntry :: FilePath -> FilePath -> AddSource -> IO Entry
createEntry base pdfsrc (FromBib bibsrc) = do
  ensure pdfsrc
  (key, title, author) <- extractBib bibsrc
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
  let matches = matchEntries query es
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

formatRow :: Int -> Int -> Int -> Entry -> Text
formatRow keyW titleW projectW e =
  padRight keyW (truncateText keyW e.key) <> "  " <>
  padRight titleW (truncateText titleW e.title) <>
  (if null e.projects then ""
    else T.pack "  [" <> truncateText projectW (comma e.projects) <> "]")
  where
    comma = T.intercalate ", "

listEntry :: [Entry] -> IO ()
listEntry es = do
  let maxKey = maximum [T.length e.key | e <- es] + 1
      es'  = sortOn key es
      rows = map (formatRow maxKey 60 15) es'
  TIO.putStrLn "  Reference list  "
  TIO.putStrLn $
    padRight (maxKey + 2) "Key"
    <> padRight 62 "Title"
    <> padRight 1 "Projects"
  TIO.putStrLn $ T.replicate (maxKey + 60 + 15) "="
  TIO.putStrLn $ T.intercalate "\n" rows


pdfDir :: FilePath -> FilePath
pdfDir base = base </> "pdfs"

bibDir :: FilePath -> FilePath
bibDir base = base </> "bibs"

pdfDest :: FilePath -> Text -> FilePath
pdfDest base key = pdfDir base </> T.unpack key <> ".pdf"

bibDest :: FilePath -> Text -> FilePath
bibDest base key = bibDir base </> T.unpack key <> ".bib"

extractEntry :: [Entry] -> [Text] -> IO ()
extractEntry es xs = do
  home <- getHomeDirectory
  let base = home </> ".Papers/"
  let matches = nub $ concatMap (flip matchEntries es) xs
      files = [ bibDest base entry.key
              | entry <- matches]
  cats <- mapM TIO.readFile files
  -- change to cat here, so we cat multiple files
  if not (null cats) then
    TIO.putStrLn $ T.intercalate "\n\n" cats
  else
    die "No entries found."

trim :: Text -> Text
trim = T.unwords . T.words . T.replace "\n\t" ""

infoEntry :: [Entry] -> Text -> IO ()
infoEntry es query = do
  home <- getHomeDirectory
  let base = home </> ".Papers/"
  let matches = nub $ matchEntries query es

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

  TIO.putStrLn $ "Type: "   <> T.pack bib.entryType

  case match.projects of
    (_:_) -> TIO.putStrLn $ "Projects: " <> T.intercalate ", " match.projects
    []    -> pure ()

  case match.keywords of
    (_:_) -> TIO.putStrLn $ "Keywords: "  <> T.intercalate ", " match.keywords
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
    List            -> listEntry stmts
    Open query      -> openEntry stmts query
    Extract query   -> extractEntry stmts query
    Edit            -> editEntry base
    Info query      -> infoEntry stmts query
    Add pdfpath bibpath -> do
      entry <- createEntry base pdfpath bibpath
      let contains' = entry `elem` stmts
      if contains'
        then die "Entry already exists."
        else do
            let newStmts = stmts ++ [entry]
                newToml  = Toml.encode entriesCodec newStmts
            TIO.writeFile (base </> "meta.toml") newToml
            putStrLn $ "Added `" ++ T.unpack entry.key ++ "` to library!"
