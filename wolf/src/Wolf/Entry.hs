module Wolf.Entry where

import Import

import qualified Data.Map as M

import Wolf.Editor
import Wolf.Index
import Wolf.Path
import Wolf.Types

entry :: String -> IO ()
entry person = do
    origIndex <- getIndex
    (personUuid, index) <- lookupOrCreateNewPerson person origIndex
    tmpFile <- tmpPersonEntryFile personUuid
    origPersonEntry <- getPersonEntryOrNew personUuid
    ensureDir $ parent tmpFile
    let tmpFileContents = tmpEntryFileContents person personUuid origPersonEntry
    writeFile (toFilePath tmpFile) tmpFileContents
    editResult <- startEditorOn tmpFile
    case editResult of
        EditingFailure reason ->
            liftIO $
            die $
            unwords
                [ "ERROR: failed to edit the note file:"
                , show reason
                , ",not saving."
                ]
        EditingSuccess -> do
            contents <- readFile $ toFilePath tmpFile
            case parseEntryFileContents contents of
                Left err -> die $ unwords ["Unable to parse entry file:", err]
                Right personEntry -> do
                    putPersonEntry personUuid personEntry
                    putIndex index

tmpEntryFileContents :: String -> PersonUuid -> PersonEntry -> String
tmpEntryFileContents person personUuid pe =
    unlines $
    map (uncurry toLineStr) (sortOn fst $ M.toList $ personEntryProperties pe) ++
    separator ++
    map
        (uncurry toLineStr)
        [("uuid", personUuidString personUuid), ("reference used", person)]
  where
    separator = ["", "", "", line, str, line]
      where
        str = "| Anything below this bar will be ignored. |"
        line = replicate (length str) '-'
    toLineStr k v = unwords [k ++ ":", v]

parseEntryFileContents :: String -> Either String PersonEntry
parseEntryFileContents str =
    (PersonEntry . M.fromList) <$>
    mapM
        parseProperty
        (filter (not . null) . takeWhile (not . isPrefixOf "---") . lines $ str)
  where
    parseProperty :: String -> Either String (String, String)
    parseProperty s =
        case break (== ':') s of
            (_, []) ->
                Left $ unwords ["Could not parse a property from", show s]
            (key, ':':val) -> Right (stripWhitespace key, stripWhitespace val)
            _ ->
                Left $
                unwords
                    [ "Something really weird happened while parsing"
                    , show s
                    , "for a property"
                    ]

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhite . reverse . dropWhite
  where
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')
