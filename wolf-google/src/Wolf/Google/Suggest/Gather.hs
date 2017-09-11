module Wolf.Google.Suggest.Gather where

import Import

import qualified Data.Text as T
import Lens.Micro

import Network.Google.People

import Wolf.Data

import Wolf.Google.Suggest.Types

gatherData :: Person -> GatheredPerson
gatherData p =
    GatheredPerson
    { gatheredPersonAliases = gatherAliases p
    , gatheredPersonNames = gatherNames p
    , gatheredPersonEmails = gatherEmails p
    , gatheredPersonPhoneNumbers = gatherPhoneNumbers p
    }

gatherAliases :: Person -> [Alias]
gatherAliases p =
    flip mapMaybe (p ^. perNames) $ \n -> do
        firstName <- n ^. nGivenName
        lastName <- n ^. nFamilyName
        pure $ alias $ T.unwords [firstName, lastName]

gatherNames :: Person -> [GatheredName]
gatherNames p =
    flip map (p ^. perNames) $ \n ->
        GatheredName
        { gatheredNamePrefix = n ^. nHonorificPrefix
        , gatheredNameFirstName = n ^. nGivenName
        , gatheredNameMiddleName = n ^. nMiddleName
        , gatheredNameLastName = n ^. nFamilyName
        , gatheredNameSuffix = n ^. nHonorificSuffix
        }

gatherEmails :: Person -> [Text]
gatherEmails p = flip mapMaybe (p ^. perEmailAddresses) $ \ea -> ea ^. eaValue

gatherPhoneNumbers :: Person -> [Text]
gatherPhoneNumbers p =
    flip mapMaybe (p ^. perPhoneNumbers) $ \pn -> pn ^. pnCanonicalForm
