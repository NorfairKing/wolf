{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Entry where

import Import

import qualified Data.Text as T
import Data.Time

import Wolf.Data.Types

personEntryTuples :: PersonEntry -> [(Text, PersonPropertyValue)]
personEntryTuples = personEntryProperties

parseFirstnameLastname :: Text -> Maybe (Text, Text)
parseFirstnameLastname s =
    case T.words s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

reconstructPersonEntry ::
       UTCTime -> PersonEntry -> [(Text, Text)] -> PersonEntry
reconstructPersonEntry now old newMap =
    if map (second personPropertyValueContents) (personEntryProperties old) ==
       newMap
        then old -- If there is no difference, don't change the last changed timestamp.
        else PersonEntry
             { personEntryProperties =
                   map (\(k, v) -> (k, go k v)) $ nubBy ((==) `on` fst) newMap
             }
  where
    go :: Text -> Text -> PersonPropertyValue
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

tmpEntryFileContents :: Text -> PersonUuid -> PersonEntry -> Text
tmpEntryFileContents person personUuid pe =
    T.unlines $
    map (uncurry toLineStr) (personEntryProperties pe) ++
    separator ++
    map (uncurry toLineStr')
        [("uuid", personUuidText personUuid), ("reference used", person)]
  where
    separator = ["", "", "", line, str, line]
      where
        str = "| Anything below this bar will be ignored. |"
        line = T.pack $ replicate (T.length str) '-'
    toLineStr k v = toLineStr' k $ personPropertyValueContents v
    toLineStr' k v = T.unwords [k <> ":", v]

parseEntryFileContents :: Text -> Either Text [(Text, Text)]
parseEntryFileContents str =
    mapM
        parseProperty
        (filter (not . T.null) . takeWhile (not . T.isPrefixOf "---") . T.lines $
         str)
  where
    parseProperty :: Text -> Either Text (Text, Text)
    parseProperty s =
        case break (== ':') $ T.unpack s of
            (_, []) ->
                Left $
                T.pack $ unwords ["Could not parse a property from", show s]
            (key, ':':val) ->
                Right
                    (T.pack $ stripWhitespace key, T.pack $ stripWhitespace val)
            _ ->
                Left $
                T.pack $
                unwords
                    [ "Something really weird happened while parsing"
                    , show s
                    , "for a property"
                    ]

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhite . reverse . dropWhite
  where
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')
