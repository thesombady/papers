{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.List ( nub, sortOn )
import Data.Maybe ( fromMaybe )

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Toml ( TomlCodec, (.=) )
import qualified Toml

import System.Exit (die)
import System.Info ( os )
import System.FilePath ( (</>), takeExtension )
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

data AddArgs = AddArgs
  { aaOnlyBib :: Bool
  , aaPdf     :: Maybe FilePath
  , aaBib     :: Maybe FilePath
  , aaDoi     :: Maybe DOI
  } deriving Show

data Context
  = List (Maybe Filter)
  | Open Text
  | Add AddArgs
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
    <> command "add"     (info addParser     (progDesc "Add a entry to the library"))
    <> command "open"    (info openParser    (progDesc "Open a single entry in `$PDF_VIEWER`"))
    <> command "list"    (info listParser    (progDesc "List all entries"))
    <> command "ls"      (info listParser    (progDesc "List all entries -- alias for list"))
    <> command "attatch" (info attatchParser (progDesc "Attatch a pdf to an existing entry"))
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
  strArgument ( metavar "Item"  <> help "Query to attatch to" ) <*>
    ( FromDOI <$> option str (long "doi" <> metavar "DOI" <> help "Fetch PDF using DOI if possible")
    <|> FromRaw <$> strArgument (metavar "PDF" <> help "PDF source file"))

editParser :: Parser Context
editParser = pure Edit

addParser :: Parser Context
addParser = Add <$> addArgsParser

addArgsParser :: Parser AddArgs
addArgsParser = AddArgs
  <$> switch
      ( long "only-bib" <> short 'b'
      <> help "Add an entry with no PDF (BibTeX required via BIB or --doi)" )
  <*> optional (strArgument
      ( metavar "PDF" <> help "PDF source file (omit with --only-bib)" ))
  <*> optional (strArgument
      ( metavar "BIB" <> help "BibTeX source file" ))
  <*> optional (T.pack <$> strOption
      ( long "doi" <> metavar "DOI" <> help "Fetch BibTeX from DOI" ))

validateAddArgs :: AddArgs -> Either Text (Maybe FilePath, AddSource)
validateAddArgs a = do
  mpdf <- case (aaOnlyBib a, aaPdf a) of
    (True,  _)        -> Right Nothing
    (False, Just pdf) -> Right (Just pdf)
    (False, Nothing)  -> Left "Missing PDF. Provide a PDF or use --only-bib."

  src <- case (aaBib a, aaDoi a) of
    (Just b, Nothing) -> Right (FromRaw b)
    (Nothing, Just d) -> Right (FromDOI d)
    (Nothing, Nothing) -> Left "Need either BIB or --doi DOI."
    (Just _, Just _)   -> Left "Use either BIB or --doi DOI (not both)."

  Right (mpdf, src)

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

ensure :: Maybe FilePath -> IO ()
ensure f = do
  case f of
    Nothing -> pure ()
    Just f' -> do
      ok <- doesFileExist f'
      if ok then pure ()
      else die  $ "File `" ++ f' ++ "` does not exist"

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
  bib <- parseBib fp
  let key'   = bib.identifier
      title  = getInfo "title" bib
      author = getInfo "author" bib

  pure  ( T.pack key'
        , title
        , author)
  where
    getInfo field bib = trim $ T.pack $ fromMaybe "Unknown" (getField field bib)


fetchBibFromDoi :: DOI -> IO Text
fetchBibFromDoi doi = do
  let cmd = "curl -fsSL -H 'Accept: application/x-bibtex' https://doi.org/"
              <> T.unpack doi
  out <- readCreateProcess (shell cmd) ""
  if null out
    then die $ "FAILED to fetch BibTex from DOI: " <> T.unpack doi
    else pure (T.pack out)

moveIntoLibrary' :: FilePath -> FilePath -> Text -> IO FilePath
moveIntoLibrary' base src key
  = let out = if takeExtension out == ".pdf"
              then pdfDest base key
              else bibDest base key
  in ensure (Just src) >> renameFile src out >> pure out

-- Rewrite since we do the majority of the same things.
createEntry :: [Entry] -> FilePath -> Maybe FilePath -> AddSource -> IO (Maybe Entry)
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
  pdfdest <- copyIntoLibrary' base pdfSrc key'
  bibdest <- copyIntoLibrary' base (Just bibSrc) key'
  case bib of
    FromDOI _ -> removeFile bibSrc
    _         -> pure ()

  case bibdest of
    Nothing       -> pure Nothing
    Just bibdest' -> pure $ Just Entry
      { key = key'
      , pdfPath = pdfdest
      , bibPath = bibdest'
      , authors = author
      , title = title
      , keywords = []
      , projects = []
      }

openCmd :: String
openCmd
  | os == "darwin" = "open"
  | otherwise      = "xdg-open"

openPdf :: Entry -> IO ()
openPdf e = do
  case e.pdfPath of
    Nothing -> die $ T.unpack $ "No affilieted pdf with " <> e.key
    fp@(Just fp') -> do
      ensure fp
      viewer <- lookupEnv "PDF_VIEWER"
      let cmd = fromMaybe openCmd viewer
      _ <- spawnProcess cmd [fp']
      pure ()

openEntry :: [Entry] -> Text -> IO ()
openEntry es query = do
  let matches = nub $ matchEntries es query
  case matches of
    []  -> die $ "No matches for: " <> T.unpack query
    [e] -> openPdf e
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
  TIO.putStrLn $ "  References (" <> T.pack (show (length es')) <> " entries)"
  TIO.putStrLn $
    padRight (maxKey + 2) "Key"
    <> padRight 62 "Title"
    <> padRight 17 "Projects"
    <> padRight 1  "Keywords"
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

  -- TODO: cleanup this
  let abstract = getField "abstract"
      fields bib' match'
        = [ "Type: "      <> T.pack bib'.entryType
          , "Title: "     <> getInfo "title" bib'
          , "Author(s): " <> getInfo "author" bib'
          , "Projects: "  <> T.intercalate ", " match'.projects
          , "Keywords: "  <> T.intercalate ", " match'.keywords
          , T.pack $ "Abstract: " ++ fromMaybe "" (abstract bib')]

  TIO.putStrLn $ T.unlines (fields bib match)
  where
    getInfo field bib = trim $ T.pack $ fromMaybe "Unknown" (getField field bib)

editEntry :: FilePath -> IO ()
editEntry base = let fp  = base </> "meta.toml"
                     cmd = fromMaybe "vi"
                  in ensure (Just fp) >> lookupEnv "EDITOR"
                    >>= (\ed -> callCommand $ cmd ed ++ " " ++ fp) >> pure ()

getPath :: FilePath -> Maybe FilePath -> Text -> Maybe FilePath
getPath base fp key = case fp of
  Nothing  -> Nothing
  Just fp' -> if takeExtension fp' == ".pdf" then Just $ pdfDest base key
              else Just $ bibDest base key

-- copy src to base </> dir </> key . extension
copyIntoLibrary' :: FilePath -> Maybe FilePath -> Text -> IO (Maybe FilePath)
copyIntoLibrary' base fp key =
  case (fp, getPath base fp key) of
    (Just in', fp'@(Just out')) -> ensure fp >> copyFile in' out' >> pure fp'
    (_,_)                       -> pure Nothing

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
  createDirectoryIfMissing True (pdfDir base)
  createDirectoryIfMissing True (bibDir base)
  ensure $ Just (base </> "meta.toml")

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

    Add args -> do
      (mpdf, src) <- case validateAddArgs args of
        Left msg -> die (T.unpack msg)
        Right x  -> pure x
    
      entry <- createEntry stmts base mpdf src
      case entry of
        Nothing     -> die "Could not add entry."
        Just entry' -> do
          writeToToml base (entry':stmts)
          TIO.putStrLn $ "Added `" <> entry'.key <> "` to library!"

    Attatch query pdf -> do
      (entry, stmts') <- case findEntryPair stmts query "__UNUSED__KEY__" of
        Left msg -> die $ T.unpack msg
        Right r  -> pure r

      entry' <- case pdf of
        FromRaw fp  -> do
          fp' <- copyIntoLibrary' base (Just fp) entry.key
          pure $ entry{pdfPath = fp'}
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
      bib <- parseBib entry.bibPath
      let bib' = bib { Bib.identifier = T.unpack nkey }
      TIO.writeFile entry.bibPath (T.pack $ BIBFormat.entry bib')

      pdfPath <- copyIntoLibrary' base entry.pdfPath nkey
      bibPath <- copyIntoLibrary' base (Just entry.bibPath) nkey

      entry' <- case bibPath of
                    Nothing -> die $ T.unpack $ "No Bib source for "
                                      <> entry.key <> " -> "  <> nkey <> "."
                    Just fp -> pure $ entry
                                  { key = nkey
                                  , pdfPath = pdfPath
                                  , bibPath = fp}

      writeToToml base (stmts' ++ [entry'])
      TIO.putStrLn $ "Renamed `" <> query <> "` to `"
                      <> nkey <> "` in the library!"
    where
      newToml = Toml.encode entriesCodec
      writeToToml base st = TIO.writeFile (base </> "meta.toml") (newToml st)

