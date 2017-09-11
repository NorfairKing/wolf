{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Google.Suggest.MakeEntry
    ( makeSuggestionProperty
    ) where

import Import

import qualified Data.Text as T
import Data.Time

import Wolf.Data

import Wolf.Google.Suggest.Types

makeSuggestionProperty :: UTCTime -> GatheredPerson -> ([Alias], PersonProperty)
makeSuggestionProperty now = suggestionProperty now . deduplicateGathering

deduplicateGathering :: GatheredPerson -> GatheredPerson
deduplicateGathering GatheredPerson {..} =
    GatheredPerson
    { gatheredPersonAliases = nub gatheredPersonAliases
    , gatheredPersonNames = nub gatheredPersonNames
    , gatheredPersonEmails = nub gatheredPersonEmails
    , gatheredPersonPhoneNumbers = nub gatheredPersonPhoneNumbers
    }

suggestionProperty :: UTCTime -> GatheredPerson -> ([Alias], PersonProperty)
suggestionProperty now GatheredPerson {..} = (aliases, PMap $ ns ++ es ++ ps)
  where
    aliases =
        map alias $
        flip mapMaybe gatheredPersonNames $ \GatheredName {..} ->
            fmap (\(fn, ln) -> T.unwords [fn, ln]) $
            (,) <$> gatheredNameFirstName <*> gatheredNameLastName
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
