{-# LANGUAGE FlexibleContexts #-}

module Wolf.Entry where

import Import

import Data.Time

import Wolf.Editor
import Wolf.Git
import Wolf.Index
import Wolf.Init
import Wolf.OptParse.Types
import Wolf.Path
import Wolf.Types

parseFirstnameLastname :: String -> Maybe (String, String)
parseFirstnameLastname s =
    case words s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

reconstructPersonEntry ::
       UTCTime -> PersonEntry -> [(String, String)] -> PersonEntry
reconstructPersonEntry now old newMap =
    if map (second personPropertyValueContents) (personEntryProperties old) ==
       newMap
        then old -- If there is no difference, don't change the last changed timestamp.
        else PersonEntry
             { personEntryProperties =
                   map (\(k, v) -> (k, go k v)) $ nubBy ((==) `on` fst) newMap
             , personEntryLastUpdatedTimestamp = now
             }
  where
    go :: String -> String -> PersonPropertyValue
    go key value =
        case lookup key (personEntryProperties old) of
            Nothing -- Key did not exist before, therefore it was created here.
             ->
                PersonPropertyValue
                { personPropertyValueContents = value
                , personPropertyValueLastUpdatedTimestamp = now
                }
            Just oldValue -- Key did exist before, have to check if there's a difference.
             ->
                PersonPropertyValue
                { personPropertyValueContents = value
                , personPropertyValueLastUpdatedTimestamp =
                      if value == personPropertyValueContents oldValue
                          then personPropertyValueLastUpdatedTimestamp oldValue
                          else now
                }

tmpEntryFileContents :: String -> PersonUuid -> PersonEntry -> String
tmpEntryFileContents person personUuid pe =
    unlines $
    map (uncurry toLineStr) (personEntryProperties pe) ++
    separator ++
    map (uncurry toLineStr')
        [("uuid", personUuidString personUuid), ("reference used", person)]
  where
    separator = ["", "", "", line, str, line]
      where
        str = "| Anything below this bar will be ignored. |"
        line = replicate (length str) '-'
    toLineStr k v = toLineStr' k $ personPropertyValueContents v
    toLineStr' k v = unwords [k ++ ":", v]

parseEntryFileContents :: String -> Either String [(String, String)]
parseEntryFileContents str =
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
