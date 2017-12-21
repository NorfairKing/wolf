{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Google.Suggest.Adapt
    ( findSimilar
    , adaptToPerson
    ) where

import Import

import Wolf.Data

import Wolf.Google.Suggest.Types

{-# ANN module ("HLint: ignore Use lambda-case" :: Text) #-}

findSimilar :: PersonEntry -> [PersonContext] -> Maybe (PersonContext, Double)
findSimilar pe =
    find ((>= 1) . snd) . sortOn (Down . snd) . map (\c -> (c, sim c))
  where
    sim :: PersonContext -> Double
    sim PersonContext {..} =
        sum
            [ inter personEntryEmails
            , inter personEntryPhoneNumbers
            , 0.6 * inter getFirstNames
            , 0.6 * inter getMiddleNames
            , 0.6 * inter getLastNames
            ]
      where
        inter func =
            genericLength (func pe `intersect` maybe [] func personContextEntry)

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
                                    (PMap vs_) ->
                                        case lookup key1 vs_ of
                                            Just (PVal v_) ->
                                                [personPropertyValueContents v_]
                                            Nothing -> []
                                            Just (PList _) -> []
                                            Just (PMap _) -> []
                        Just (PMap vs_) ->
                            case lookup key1 vs_ of
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

adaptToPerson ::
       [Alias]
    -> PersonEntry
    -> PersonContext
    -> Maybe ([Alias], PersonProperty)
adaptToPerson aliases pe pc =
    case personContextEntry pc of
        Nothing -> Just (aliases, pp1)
        Just pp2 ->
            (,) (aliases \\ personContextAliases pc) <$>
            go pp1 (personEntryProperties pp2)
  where
    pp1 = personEntryProperties pe
    go :: PersonProperty -> PersonProperty -> Maybe PersonProperty
    go p1 p2 =
        if sameValues p1 p2
            then Nothing
            else case (p1, p2) of
                     (PVal _, PVal _) -> Just p1
                     (PList _, PList _) -> Just p1
                     (PMap m1, PMap m2) ->
                         case deleteFirstsBy tuplesMatch m1 m2 of
                             [] -> Nothing
                             kvs -> Just $ PMap kvs
                     _ -> Just p1
    tuplesMatch (k1, v1) (k2, v2) = k1 == k2 && sameValues v1 v2
