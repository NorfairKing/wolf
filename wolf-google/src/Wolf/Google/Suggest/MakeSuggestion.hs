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
makeSuggestions now _ gp = sug <$> personEntry (suggestionProperty now gp)

suggestionProperty :: UTCTime -> GatheredPerson -> PersonProperty
suggestionProperty now GatheredPerson {..} =
    PMap
        [ ( "name"
          , PList $
            flip map gatheredPersonNames $ \n ->
                let mval tk func = fmap (val now) (func n)
                in PList $ catMaybes [mval "first name" gatheredNamePrefix])
        , ("email", PList $ map (val now) gatheredPersonEmails)
        , ("phone number", PList $ map (val now) gatheredPersonPhoneNumbers)
        ]

val :: UTCTime -> Text -> PersonProperty
val now t =
    PVal $
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
