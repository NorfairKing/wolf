{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Google.Suggest.MakeSuggestion where

import Import

import qualified Data.Text as T
import Data.Time

import Wolf.Data

import Wolf.Google.Suggest.Types

makeSuggestions ::
       UTCTime
    -> [PersonContext]
    -> GatheredPerson
    -> Maybe (Suggestion EntrySuggestion)
makeSuggestions now pcs gp' = do
    let gp = deduplicateGathering gp'
        prop = suggestionProperty now gp
    pe' <- personEntry prop
    let bySim = findSimilar pe' pcs
    let suggestor = "Google Contacts"
    pure $
        traceShow bySim $
        traceShow pe' $
        case bySim of
            Nothing ->
                Suggestion
                { suggestionSuggestor = suggestor
                , suggestionReason = "This data was exported"
                , suggestionData =
                      EntrySuggestion
                      { entrySuggestionEntry = pe'
                      , entrySuggestionLikelyRelevantPerson = Nothing
                      }
                }
            Just (pc, s) ->
                let pe = adaptToPerson pe' pc
                    uuid = personContextUuid pc
                in Suggestion
                   { suggestionSuggestor = suggestor
                   , suggestionReason =
                         "This data was exported and suggested to be related to the person with UUID " <>
                         personUuidText uuid <>
                         " and score " <>
                         T.pack (show s) <> "."
                   , suggestionData =
                         EntrySuggestion
                         { entrySuggestionEntry = pe
                         , entrySuggestionLikelyRelevantPerson = Just (uuid, s)
                         }
                   }

deduplicateGathering :: GatheredPerson -> GatheredPerson
deduplicateGathering GatheredPerson {..} =
    GatheredPerson
    { gatheredPersonAliases = nub gatheredPersonAliases
    , gatheredPersonNames = nub gatheredPersonNames
    , gatheredPersonEmails = nub gatheredPersonEmails
    , gatheredPersonPhoneNumbers = nub gatheredPersonPhoneNumbers
    }

suggestionProperty :: UTCTime -> GatheredPerson -> PersonProperty
suggestionProperty now GatheredPerson {..} = PMap $ ns ++ es ++ ps
  where
    singleOrListMap ::
           Text -> [[(Text, PersonProperty)]] -> [(Text, PersonProperty)]
    singleOrListMap _ [] = []
    singleOrListMap _ [vs] = vs
    singleOrListMap key vs = [(key, PList $ map PMap vs)]
    ns =
        singleOrListMap "name" $
        flip map gatheredPersonNames $ \n ->
            let mval tk func = (,) tk <$> (val now <$> func n)
            in catMaybes
                   [ mval "prefix" gatheredNamePrefix
                   , mval "first name" gatheredNameFirstName
                   , mval "middle name" gatheredNameMiddleName
                   , mval "last name" gatheredNameLastName
                   , mval "suffix" gatheredNameSuffix
                   ]
    singleOrList _ [] = []
    singleOrList key [v] = [(key, v)]
    singleOrList key vs = [(key, PList vs)]
    es = singleOrList "email" $ map (val now) gatheredPersonEmails
    ps = singleOrList "phone number" $ map (val now) gatheredPersonPhoneNumbers

val :: UTCTime -> Text -> PersonProperty
val now t =
    PVal
        PersonPropertyValue
        { personPropertyValueLastUpdatedTimestamp = now
        , personPropertyValueContents = t
        }

findSimilar :: PersonEntry -> [PersonContext] -> Maybe (PersonContext, Double)
findSimilar pe =
    find ((>= 1) . snd) . sortOn (Down . snd) . map (\c -> (c, sim c))
  where
    sim :: PersonContext -> Double
    sim PersonContext {..} =
        sum
            [ inter personEntryEmails
            , inter personEntryPhoneNumbers
            , 0.6 * (inter getFirstNames)
            , 0.6 * (inter getMiddleNames)
            , 0.6 * (inter getLastNames)
            ]
      where
        inter func =
            genericLength (func pe `intersect` maybe [] func personContextEntry)

adaptToPerson :: PersonEntry -> PersonContext -> PersonEntry
adaptToPerson pc _ = pc

getFirstNames :: PersonEntry -> [Text]
getFirstNames = possiblyMultipleAt2 "name" "first name"

getMiddleNames :: PersonEntry -> [Text]
getMiddleNames = possiblyMultipleAt2 "name" "middle name"

getLastNames :: PersonEntry -> [Text]
getLastNames = possiblyMultipleAt2 "name" "last name"

possiblyMultipleAt2 :: Text -> Text -> PersonEntry -> [Text]
possiblyMultipleAt2 key2 key1 pe =
    case personEntryProperties pe of
        PVal _ -> []
        PList _ -> []
        PMap kvs ->
            case lookup key1 kvs of
                Just (PVal v) -> [personPropertyValueContents v]
                Just (PList _) -> []
                Just (PMap _) -> []
                Nothing ->
                    case lookup key2 kvs of
                        Nothing -> []
                        Just (PVal _) -> []
                        Just (PList vs) ->
                            flip concatMap vs $ \v ->
                                case v of
                                    (PVal _) -> []
                                    (PList _) -> []
                                    (PMap vs) ->
                                        case lookup key1 kvs of
                                            Just (PVal v) ->
                                                [personPropertyValueContents v]
                                            Nothing -> []
                                            Just (PList _) -> []
                                            Just (PMap _) -> []
                        Just (PMap vs) ->
                            case lookup key1 kvs of
                                Just (PVal v) -> [personPropertyValueContents v]
                                Nothing -> []
                                Just (PList _) -> []
                                Just (PMap _) -> []

personEntryEmails :: PersonEntry -> [Text]
personEntryEmails = possiblyMultipleAt "email"

personEntryPhoneNumbers :: PersonEntry -> [Text]
personEntryPhoneNumbers = possiblyMultipleAt "phone number"

possiblyMultipleAt :: Text -> PersonEntry -> [Text]
possiblyMultipleAt key pe =
    case personEntryProperties pe of
        PVal _ -> []
        PList _ -> []
        PMap kvs ->
            case lookup key kvs of
                Nothing -> []
                Just (PVal v) -> [personPropertyValueContents v]
                Just (PList vs) ->
                    mapMaybe (fmap personPropertyValueContents . onValue) vs
                Just (PMap vs) ->
                    mapMaybe
                        (fmap personPropertyValueContents . onValue . snd)
                        vs
  where
    onValue (PVal v) = Just v
    onValue _ = Nothing
