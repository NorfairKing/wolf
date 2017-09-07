{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Google.Suggest.MakeSuggestion where

import Import

import Data.Time

import Wolf.Data

import Wolf.Google.Suggest.Types

makeSuggestions ::
       UTCTime
    -> [PersonContext]
    -> GatheredPerson
    -> Maybe (Suggestion PersonEntry)
makeSuggestions now _ gp' =
    let gp = deduplicateGathering gp'
        prop = suggestionProperty now gp
    in sug <$> personEntry prop

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

sug :: PersonEntry -> Suggestion PersonEntry
sug pe =
    Suggestion
    { suggestionSuggestor = "Google Contacts"
    , suggestionReason =
          "This information was available in your google contacts data."
    , suggestionData = pe
    }
