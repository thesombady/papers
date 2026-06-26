{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, isNothing)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Toml (TomlCodec, (.=))
import qualified Toml

import System.Console.Terminal.Size
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    , removeFile
    , renameFile
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die, exitSuccess)
import System.FilePath (takeExtension, (</>))
import System.Info (os)
import System.Process
    ( callCommand
    , proc
    , readCreateProcess
    , readProcessWithExitCode
    , spawnProcess
    )

import Control.Monad (unless, when)
import Options.Applicative

import qualified Text.BibTeX.Entry as Bib
import qualified Text.BibTeX.Format as BIBFormat
import qualified Text.BibTeX.Parse as BibParse
import Text.Parsec.String (parseFromFile)

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:?))
import qualified Data.ByteString.Lazy.Char8 as LBS

type DOI = Text
type ArXiv = Text

data Format = BibF | PdfF

-- PDF fetching if possible
data OALocation = OALocation {urlForPdf :: Maybe String}

instance FromJSON OALocation where
    parseJSON = withObject "OALocation" $ \o ->
        OALocation <$> o .:? "url_for_pdf"

data UnpaywallResponse = UnpaywallResponse
    {bestOaLocation :: Maybe OALocation}

instance FromJSON UnpaywallResponse where
    parseJSON = withObject "UnpaywallResponse" $ \o ->
        UnpaywallResponse <$> o .:? "best_oa_location"

fetchOAPdfUrlFromUnpaywall :: DOI -> IO (Maybe String)
fetchOAPdfUrlFromUnpaywall doi = do
    mMail <- lookupEnv "UNPAYWALL_EMAIL"
    mail <- maybe (die "Set enviroment variable `UNPAYWALL_EMAIL` to use this feature") pure mMail
    let url = "https://api.unpaywall.org/v2/" <> T.unpack doi <> "?email=" <> mail
    json <- readCreateProcess (proc "curl" ["-fsSL", url]) ""
    case eitherDecode (LBS.pack json) of
        Left err -> die ("Failed to parse Unpaywall JSON: " <> err)
        Right (UnpaywallResponse mBest) -> pure (mBest >>= urlForPdf)

fetchPdf :: AddSource -> IO (Maybe String)
fetchPdf (FromDOI doi) = fetchOAPdfUrlFromUnpaywall doi
fetchPdf (FromArXiv arXiv) = undefined
fetchPdf (FromRaw _) = undefined

downloadPdfTo :: Maybe String -> FilePath -> IO (Maybe FilePath)
downloadPdfTo pdfUrl out = case pdfUrl of
    Nothing -> pure Nothing
    Just url ->
        readProcessWithExitCode
            "curl"
            [ "-fL"
            , "--retry"
            , "3"
            , "--retry-connrefused"
            , "-o"
            , out
            , url
            ]
            ""
            >>= \(ec, _stdout, _stderr) ->
                case ec of
                    ExitSuccess -> pure (Just out)
                    ExitFailure _ -> pure Nothing

-- RELEASE: 0.5.0. Add `add --library pdfFolder library.bib`

orDie :: Either Text a -> IO a
orDie = either (die . T.unpack) pure

data AddSource
    = FromRaw FilePath
    | FromDOI DOI
    | FromArXiv ArXiv
    deriving (Show)

data Filter
    = ByAuthor Text
    | ByProject Text
    | ByQuery [Text]
    deriving (Show)

data ExtractFilter
    = ExtractFilter Filter
    | ExtractAll
    deriving (Show)

data AddArgs = AddArgs
    { aaOnlyBib' :: Bool
    , aaPdf' :: Maybe FilePath
    , aaBib' :: AddSource
    }
    deriving (Show)

data Context
    = List (Maybe Filter)
    | Open Text
    | Add AddArgs
    | Extract ExtractFilter
    | Edit
    | Fetch AddSource
    | Attatch Text AddSource
    | Info Text
    | Rename Text Text
    | Remove Bool Text
    deriving (Show)

optionParser :: Parser Context
optionParser =
    hsubparser
        ( command "extract" (info extractParser (progDesc "Extract entries and cat to stdout"))
            <> command "get" (info extractParser (progDesc "Extract entries and cat to stdout -- alias to extract"))
            <> command "add" (info addParser (progDesc "Add a entry to the library"))
            <> command "fetch" (info fetchParser (progDesc "Try to fetch pdf and bib entry online"))
            <> command "open" (info openParser (progDesc "Open a single entry in `$PDF_VIEWER`"))
            <> command "list" (info listParser (progDesc "List all entries"))
            <> command "ls" (info listParser (progDesc "List all entries -- alias for list"))
            <> command "attatch" (info attatchParser (progDesc "Attatch a pdf to an existing entry"))
            <> command "edit" (info editParser (progDesc "Edit the metafile in $EDITOR or vi"))
            <> command "info" (info infoParser' (progDesc "Obtain information about the query result"))
            <> command "rename" (info renameParser (progDesc "Rename an entry"))
            <> command "mv" (info renameParser (progDesc "Rename (move) an entry -- alias for rename"))
            <> command "remove" (info removeParser (progDesc "Remove an entry"))
            <> command "rm" (info removeParser (progDesc "Remove an entry -- alias for remove"))
        )

extractParser :: Parser Context
extractParser = Extract <$> extractParserFilter

removeParser :: Parser Context
removeParser =
    Remove
        <$> switch (long "force" <> help "Forcefully remove the item")
        <*> (T.pack <$> strArgument (metavar "ITEM" <> help "Key/query to remove"))

extractParserFilter :: Parser ExtractFilter
extractParserFilter =
    flag' ExtractAll (long "all" <> help "Extract all entries")
        <|> ExtractFilter
        <$> entryFilterParser

entryFilterParser :: Parser Filter
entryFilterParser =
    ByProject
        <$> option
            str
            (long "project" <> metavar "PROJECT" <> help "Filter by project")
            <|> ByAuthor
        <$> option
            str
            (long "author" <> metavar "AUTHOR" <> help "Filter by author substring")
            <|> ByQuery
        <$> some
            ( strArgument
                (metavar "ITEMS..." <> help "Keys/queries to match")
            )

infoParser' :: Parser Context
infoParser' =
    Info
        <$> strArgument
            (metavar "Item" <> help "Query an entry for more information")

renameParser :: Parser Context
renameParser =
    Rename
        <$> strArgument (metavar "Item" <> help "Query to rename")
        <*> strArgument (metavar "Item" <> help "New key")

attatchParser :: Parser Context
attatchParser =
    Attatch
        <$> strArgument (metavar "Item" <> help "Query to attatch to")
        <*> ( FromDOI
                <$> option str (long "doi" <> metavar "DOI" <> help "Fetch PDF using DOI if possible")
                    <|> FromRaw
                <$> strArgument (metavar "PDF" <> help "PDF source file")
            )

editParser :: Parser Context
editParser = pure Edit

addParser :: Parser Context
addParser = Add <$> addArgsParser

fetchParser :: Parser Context
fetchParser =
    Fetch
        <$> ( FromDOI
                <$> option str (long "doi" <> metavar "DOI" <> help "DOI to fetch")
                    <|> FromArXiv
                <$> option str (long "arxiv" <> metavar "ArXiv" <> help "ArXiv to fetch")
            )

addArgsParser :: Parser AddArgs
addArgsParser =
    AddArgs
        <$> switch
            ( long "only-bib"
                <> short 'b'
                <> help "Add an entry with no PDF (BibTeX required via BIB or --doi)"
            )
        <*> optional
            ( strArgument
                (metavar "PDF" <> help "PDF source file (omit with --only-bib)")
            )
        <*> ( FromDOI
                <$> option str (long "doi" <> metavar "DOI" <> help "Fetch Bib using DOI if possible")
                    <|> FromRaw
                <$> strArgument (metavar "PDF" <> help "Bib source file")
            )

validateAddArgs :: AddArgs -> Either Text (Maybe FilePath, AddSource)
validateAddArgs a = do
    mpdf <- case (aaOnlyBib' a, aaPdf' a) of
        (True, _) -> Right Nothing
        (False, Just pdf) -> Right (Just pdf)
        (False, Nothing) -> Left "Missing PDF. Provide a PDF or use --only-bib."
    Right (mpdf, a.aaBib')

openParser :: Parser Context
openParser = Open <$> strArgument (metavar "Item" <> help "Entry to open")

listParser :: Parser Context
listParser = List <$> optional entryFilterParser

ctxInfo :: ParserInfo Context
ctxInfo =
    info
        (optionParser <**> helper)
        ( fullDesc
            <> progDesc "A cli reference manager that fits the unix philosophy."
            <> header "papers - A cli reference manger."
        )

data Entry = Entry
    { key :: Text
    , pdfPath :: Maybe FilePath
    , bibPath :: FilePath
    , authors :: Text
    , title :: Text
    , keywords :: [Text]
    , projects :: [Text]
    }
    deriving (Show)

instance Eq Entry where
    (==) e1 e2 = e1.key == e2.key

matchEntry :: Text -> Entry -> Bool
matchEntry query e
    | query == T.toCaseFold e.key = True
    | prefix' e.key = True
    | any prefix' e.keywords = True
    | infix' e.authors = True
    | infix' e.title = True
    | otherwise = False
  where
    prefix' q = query `T.isPrefixOf` T.toCaseFold q
    infix' q = query `T.isInfixOf` T.toCaseFold q

matchEntries :: [Entry] -> Text -> [Entry]
matchEntries es query =
    filter
        ( matchEntry
            ((T.toCaseFold . T.strip) query)
        )
        es

entryCodec :: TomlCodec Entry
entryCodec =
    Entry
        <$> Toml.text "key" .= key
        <*> Toml.dioptional (Toml.string "pdf") .= pdfPath
        <*> Toml.string "bib" .= bibPath
        <*> Toml.text "authors" .= authors
        <*> Toml.text "title" .= title
        <*> Toml.arrayOf Toml._Text "keywords" .= keywords
        <*> Toml.arrayOf Toml._Text "projects" .= projects

entriesCodec :: TomlCodec [Entry]
entriesCodec = Toml.list entryCodec "entry"

ensure :: Maybe FilePath -> IO ()
ensure = maybe (pure ()) $ \f ->
    doesFileExist f
        >>= flip
            unless
            (die $ "File `" ++ f ++ "` does not exist.")

type BibEntry = Bib.T

getField :: String -> BibEntry -> Maybe String
getField query e = lookup query e.fields

parseBib :: FilePath -> IO BibEntry
parseBib fp =
    parseFromFile BibParse.file fp >>= \case
        Left err -> fail (show err)
        Right es -> case es of
            (e : _) -> pure e
            [] -> die ("No BibTex entries in " ++ fp)

extractBib :: FilePath -> IO (Text, Text, Text)
extractBib fp = parseBib fp >>= \x -> pure $ getFields' x
  where
    getFields' b = (T.pack b.identifier, getInfo "title" b, getInfo "author" b)
    getInfo field bib = trim $ T.pack $ fromMaybe "Unknown" (getField field bib)

fetchBibFromDoi :: DOI -> IO Text
fetchBibFromDoi doi = do
    let url = "https://doi.org/" <> T.unpack doi
        args = ["-fsSL", "-H", "Accept: application/x-bibtex", url]
    readCreateProcess (proc "curl" args) "" >>= \out ->
        if null out
            then die $ "FAILED to fetch BibTex from DOI: " <> T.unpack doi
            else pure (T.pack out)

-- TODO: This is not the correct API
fetchBibFromArXiv :: ArXiv -> IO Text
fetchBibFromArXiv arXiv = do
    let doi =
            "10.48550/arXiv."
                <> T.unpack
                    (T.dropWhile (== ' ') arXiv)
    -- ( (T.dropWhile (== ' ') . T.stripPrefix "arXiv: ") arXiv)

    let url = "https://doi.org/" <> doi
    bib <-
        readCreateProcess
            ( proc
                "curl"
                [ "-fsSL"
                , "-H"
                , "Accept: application/x-bibtex"
                , url
                ]
            )
            ""
    if null bib
        then die $ "FAILED to fetch BibTex from ArXiv: " <> T.unpack arXiv
        else pure $ T.pack bib

-- let url  = "https://doi.org/" <> T.unpack arXiv
--     args = ["-fsSL", "-H", "Accept: application/x-bibtex", url]
-- readCreateProcess (proc "curl" args) "" >>= \out ->
--   if null out
--     then die $ "FAILED to fetch BibTex from ArXiv: " <> T.unpack arXiv
--     else pure (T.pack out)

fetchBib :: AddSource -> IO Text
fetchBib (FromDOI doi) = fetchBibFromDoi doi
fetchBib (FromArXiv arxiv) = fetchBibFromArXiv arxiv
fetchBib (FromRaw _) = die "Should never reach this point"

getFmt :: Format -> FilePath -> Text -> FilePath
getFmt BibF base key = bibDest base key
getFmt PdfF base key = pdfDest base key

moveIntoLibrary' :: FilePath -> FilePath -> Format -> Text -> IO FilePath
moveIntoLibrary' base src fmt key =
    let out = getFmt fmt base key
    in  ensure (Just src) >> renameFile src out >> pure out

getResponseFromUser :: IO Bool
getResponseFromUser = (`elem` ['y', 'Y']) <$> getChar

-- Rewrite since we do the majority of the same things.
-- TODO: also createTempFile should exist, to avoid collissions
createEntry :: [Entry] -> FilePath -> Maybe FilePath -> AddSource -> IO (Maybe Entry)
createEntry stmts base pdfSrc bib = do
    -- ensure pdfSrc
    bibSrc <- case bib of
        FromRaw r -> pure r
        FromArXiv _ -> die $ T.unpack "Not implemented yet"
        FromDOI doi -> do
            bibEntry <- fetchBibFromDoi doi
            TIO.writeFile "temp_file.bib" bibEntry
            pure "temp_file.bib"
    (key', title, author) <- extractBib bibSrc
    when (key' `elem` map key stmts) $
        die $
            T.unpack $
                "Key already exists: "
                    <> key'
                    <> ". Consider renaming the key in `temp_file.bib`."
    pdfdest <- copyIntoLibrary' base pdfSrc key'
    bibdest <- copyIntoLibrary' base (Just bibSrc) key'
    case bib of
        FromRaw _ -> pure ()
        _ -> removeFile bibSrc

    case bibdest of
        Nothing -> pure Nothing
        Just bibdest' ->
            pure $
                Just
                    Entry
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
    | otherwise = "xdg-open"

openPdf :: Entry -> IO ()
openPdf e = do
    case e.pdfPath of
        Nothing -> die $ T.unpack ("No affilieted pdf with " <> e.key)
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
        [] -> die $ "No matches for: " <> T.unpack query
        [e] -> openPdf e
        xs ->
            die $
                T.unpack $
                    "Multiple matches for: "
                        <> query
                        <> "\nMatches: "
                        <> T.intercalate ", " (map key xs)

filterEntries :: [Entry] -> Maybe Filter -> [Entry]
filterEntries es filter' =
    case filter' of
        Nothing -> es
        Just f -> case f of
            ByQuery qs -> nub $ concatMap (matchEntries es) qs
            ByProject p ->
                filter (\e -> T.toCaseFold p `elem` map T.toCaseFold e.projects) es
            ByAuthor a ->
                filter (\e -> T.toCaseFold a `T.isInfixOf` T.toCaseFold e.authors) es

-- listEntry :: [Entry] -> Maybe Filter -> IO ()
-- listEntry es filter' = do
--     let maxKey = maximum (1 : [T.length e.key | e <- es]) + 1
--         es' = sortOn (T.toCaseFold . key) (filterEntries es filter')
--         rows = map (formatRow maxKey 60 15 15) es'
--     TIO.putStrLn $ "  References (" <> T.pack (show (length es')) <> " entries)"
--     TIO.putStrLn $
--         padRight (maxKey + 2) "Key"
--             <> padRight 62 "Title"
--             <> padRight 17 "Projects"
--             <> padRight 1 "Keywords"
--     TIO.putStrLn $ T.replicate (maxKey + 60 + 15 * 2 + 8) "="
--     TIO.putStrLn $ T.intercalate "\n" rows

formatList :: [Text] -> Text
formatList [] = ""
formatList xs =
    "[" <> T.intercalate ", " xs <> "]"

sep :: Text
sep = " "

formatRow :: Int -> Int -> Int -> Int -> Entry -> T.Text
formatRow keyW titleW projectW keywordW e =
    padRight keyW (key e)
        <> sep
        <> padRight titleW (title e)
        <> sep
        <> padRight projectW (formatList (projects e))
        <> sep
        <> padRight keywordW (formatList (keywords e))

padRight :: Int -> Text -> Text
padRight n t =
    let t' = crop n t
    in  t' <> T.replicate (max 0 (n - T.length t')) " "

crop :: Int -> Text -> Text
crop n t
    | n <= 0 = ""
    | T.length t <= n = t
    | n <= 1 = "…"
    | otherwise = T.take (n - 1) t <> "…"

listEntry :: [Entry] -> Maybe Filter -> IO ()
listEntry es filter' = do
    mWin <- size
    let termWidth = maybe 100 width mWin

        es' = sortOn (T.toCaseFold . key) (filterEntries es filter')

        keyW = maximum (1 : [T.length (key e) | e <- es']) + 2
        projectW = 15
        keywordW = 25

        minTitleW = 20
        prefTitleW = 120

        availableTitleW = termWidth - keyW - projectW - keywordW - 3

    if availableTitleW < minTitleW
        then
            TIO.putStrLn $
                "Terminal too small. Need at least "
                    <> T.pack (show (keyW + minTitleW + projectW + keywordW))
                    <> " columns, have "
                    <> T.pack (show termWidth)
                    <> "."
        else do
            let titleW = min prefTitleW availableTitleW
                totalW = keyW + titleW + projectW + keywordW
                rows = map (formatRow keyW titleW projectW keywordW) es'

            TIO.putStrLn $
                "  References (" <> T.pack (show (length es')) <> " entries)"

            TIO.putStrLn $
                padRight (keyW + 1) "Key"
                    <> padRight (titleW + 1) "Title"
                    <> padRight (projectW + 1) "Projects"
                    <> padRight (keywordW) "Keywords"

            TIO.putStrLn $ T.replicate (totalW + 2) "="
            TIO.putStrLn $ T.intercalate "\n" rows

pdfDir :: FilePath -> FilePath
pdfDir base = base </> "pdfs"

bibDir :: FilePath -> FilePath
bibDir base = base </> "bibs"

pdfDest :: FilePath -> Text -> FilePath
pdfDest base key = pdfDir base </> T.unpack key <> ".pdf"

bibDest :: FilePath -> Text -> FilePath
bibDest base key = bibDir base </> T.unpack key <> ".bib"

extractEntry :: FilePath -> [Entry] -> ExtractFilter -> IO ()
extractEntry base es query = do
    let matches = case query of
            ExtractFilter query' -> filterEntries es (Just query')
            ExtractAll -> es
        files = [bibDest base entry.key | entry <- matches]
    mapM TIO.readFile files >>= \cats' ->
        if not (null cats')
            then TIO.putStrLn $ T.intercalate "\n\n" cats'
            else die "No entries found"

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
        [] -> die $ T.unpack $ "No matches for" <> query
        xs ->
            die $
                T.unpack $
                    "Multiple results for: "
                        <> query
                        <> "\nMatches: "
                        <> T.intercalate ", " (map key xs)

    let fp = bibDest base match.key
    parseBib fp >>= \bib -> TIO.putStrLn $ T.unlines (fields bib match)
  where
    getInfo field bib' = trim $ T.pack $ fromMaybe "Unknown" (getField field bib')
    fields bib' match' =
        [ "Type: " <> T.pack bib'.entryType
        , "Title: " <> getInfo "title" bib'
        , "Author(s): " <> getInfo "author" bib'
        , "Projects: " <> T.intercalate ", " match'.projects
        , "Keywords: " <> T.intercalate ", " match'.keywords
        , T.pack $ "Abstract: " ++ fromMaybe "" (getField "abstract" bib')
        ]

editEntry :: FilePath -> IO ()
editEntry base =
    let fp = base </> "meta.toml"
    in  ensure (Just fp)
            >> lookupEnv "EDITOR"
            >>= \ed -> callCommand $ fromMaybe "vi" ed ++ " " ++ fp

getPath :: FilePath -> Maybe FilePath -> Text -> Maybe FilePath
getPath base fp key = case fp of
    Nothing -> Nothing
    Just fp' ->
        if takeExtension fp' == ".pdf"
            then Just $ pdfDest base key
            else Just $ bibDest base key

-- copy src to base </> dir </> key . extension
copyIntoLibrary' :: FilePath -> Maybe FilePath -> Text -> IO (Maybe FilePath)
copyIntoLibrary' base fp key =
    case (fp, getPath base fp key) of
        (Just in', fp'@(Just out')) -> ensure fp >> copyFile in' out' >> pure fp'
        (_, _) -> pure Nothing

findEntryPair :: [Entry] -> Text -> Text -> Either Text (Entry, [Entry])
findEntryPair es query nkey =
    let matches = nub $ matchEntries es query
        contained = any (\x -> x.key == nkey) es
    in  case (matches, contained) of
            ([], False) -> Left "No match"
            ([], True) -> Left "No match, but key already exists"
            ([match], False) -> Right (match, filter (/= match) es)
            (_, False) ->
                Left $
                    "Multiple matches for `"
                        <> query
                        <> "`: "
                        <> T.intercalate ", " (map key matches)
            (_, True) ->
                Left $
                    "Key "
                        <> nkey
                        <> " already exists, and found multiple matches for `"
                        <> query
                        <> "`: "
                        <> T.intercalate ", " (map key matches)

attachEntry :: FilePath -> [Entry] -> Text -> AddSource -> IO [Entry]
attachEntry base stmts query pdf =
    case findEntryPair stmts query "__UNUSED__KEY__" of
        Left msg -> die $ T.unpack msg
        Right (entry, stmts') -> case pdf of
            FromArXiv _ -> die "Not implemented yet"
            FromRaw fp ->
                copyIntoLibrary' base (Just fp) entry.key
                    >>= (\fp' -> pure $ entry{pdfPath = fp'} : stmts')
            FromDOI doi -> do
                let fp = pdfDest base entry.key
                _ <- fetchOAPdfUrlFromUnpaywall doi >>= \url -> downloadPdfTo url fp
                pure $ entry{pdfPath = Just fp} : stmts'

addEntry :: FilePath -> [Entry] -> AddArgs -> IO [Entry]
addEntry base stmts args =
    either
        (die . T.unpack)
        (uncurry (createEntry stmts base))
        (validateAddArgs args)
        >>= maybe
            (die "Could not add entry.")
            (pure . (: stmts))

fetchEntry :: FilePath -> [Entry] -> AddSource -> IO [Entry]
fetchEntry base stmts add = do
    bibSource <-
        (fetchBib add >>= TIO.writeFile "temp_file.bib")
            >> pure "temp_file.bib"
    (key', title, author) <- extractBib bibSource
    when (key' `elem` map key stmts) $
        die $
            T.unpack $
                "Key already exists: "
                    <> key'
                    <> ". Consider renaming the key in `temp_file.bib`"
    url <- fetchPdf add
    bibdest <- copyIntoLibrary' base (Just bibSource) key'
    removeFile bibSource

    case bibdest of
        Nothing -> die "Should never come to this."
        Just bib' -> do
            pdf' <- downloadPdfTo url (pdfDest base key')
            -- when (isNothing pdf') $ -- TODO: add query to proceed. getResponseFromUser ...
            --   TIO.putStrLn ("Failed to get the pdf" <> key' <> ", do you want to proceed?")
            --     >> getResponseFromUser >>= maybe (die "Terminating") ()
            pure
                ( Entry
                    { key = key'
                    , pdfPath = pdf'
                    , bibPath = bib'
                    , authors = author
                    , title = title
                    , keywords = []
                    , projects = []
                    }
                    : stmts
                )

removeEntry :: [Entry] -> Bool -> Text -> IO [Entry]
removeEntry stmts mode query = do
    (entry', stmts') <- orDie (findEntryPair stmts query query)

    let askAction =
            TIO.putStrLn ("Do you want to remove " <> entry'.key <> "? y/N")
                >> getResponseFromUser

        removeIfExists fp =
            doesFileExist fp >>= \ok -> when ok (removeFile fp)

        removeAction =
            maybe (pure ()) removeIfExists entry'.pdfPath
                >> removeIfExists entry'.bibPath
                >> pure stmts'

    ok <- if mode then pure True else askAction
    if ok then removeAction else exitSuccess

renameEntry :: FilePath -> [Entry] -> Text -> Text -> IO [Entry]
renameEntry base stmts query nkey = do
    (entry, stmts') <- case findEntryPair stmts query nkey of
        Left msg -> die $ T.unpack msg
        Right r -> pure r
    bib <- parseBib entry.bibPath
    let bib' = bib{Bib.identifier = T.unpack nkey}
    TIO.writeFile entry.bibPath (T.pack $ BIBFormat.entry bib')

    pdfPath <- copyIntoLibrary' base entry.pdfPath nkey
    copyIntoLibrary' base (Just entry.bibPath) nkey >>= \case
        Nothing -> die $ T.unpack $ "No Bib source for " <> entry.key <> " -> " <> nkey <> "."
        Just fp ->
            pure
                ( entry
                    { key = nkey
                    , pdfPath = pdfPath
                    , bibPath = fp
                    }
                    : stmts'
                )

runPapers :: FilePath -> [Entry] -> Context -> IO ()
runPapers base stmts ctx = case ctx of
    List filter' -> listEntry stmts filter'
    Open query -> openEntry stmts query
    Extract query -> extractEntry base stmts query
    Edit -> editEntry base
    Info query -> infoEntry stmts query
    Add args ->
        addEntry base stmts args
            >>= writeToToml
            >> TIO.putStrLn "Added to the library"
    Fetch query ->
        fetchEntry base stmts query
            >>= writeToToml
            >> TIO.putStrLn "Added to the library"
    Attatch query pdf ->
        (attachEntry base stmts query pdf >>= writeToToml)
            >> TIO.putStrLn ("Attached pdf to " <> query)
    Remove mode query ->
        removeEntry stmts mode query
            >>= writeToToml
            >> TIO.putStrLn ("Removed " <> query)
    Rename query nkey ->
        renameEntry base stmts query nkey
            >>= writeToToml
            >> TIO.putStrLn ("Renamed " <> query <> " to " <> nkey)
  where
    newToml = Toml.encode entriesCodec
    writeToToml st = TIO.writeFile (base </> "meta.toml") (newToml st)

main :: IO ()
main = do
    home <- getHomeDirectory
    let base = home </> ".Papers"
    createDirectoryIfMissing True (pdfDir base)
    createDirectoryIfMissing True (bibDir base)
    ensure $ Just (base </> "meta.toml")

    input <- TIO.readFile $ base </> "meta.toml"
    stmts <- case Toml.decode entriesCodec input of
        Left msgs -> die (T.unpack $ Toml.prettyTomlDecodeErrors msgs)
        Right entries -> pure entries
    ctx <- execParser ctxInfo
    runPapers base stmts ctx
