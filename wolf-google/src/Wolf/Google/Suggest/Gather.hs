module Wolf.Google.Suggest.Gather where

import Import

import Data.List
import qualified Data.Text as T
import Lens.Micro

import Network.Google.People

import Wolf.Google.Suggest.Types

gatherData :: Person -> GatheredPerson
gatherData p =
    GatheredPerson
    { gatheredPersonAliases = gatherAliases p
    , gatheredPersonNames = gatherNames p
    , gatheredPersonEmails = gatherEmails p
    , gatheredPersonPhoneNumbers = gatherPhoneNumbers p
    }

gatherAliases :: Person -> [Text]
gatherAliases p =
    nub $
    flip mapMaybe (p ^. perNames) $ \n -> do
        firstName <- n ^. nGivenName
        lastName <- n ^. nFamilyName
        pure $ T.unwords [firstName, lastName]

gatherNames :: Person -> [GatheredName]
gatherNames p =
    nub $
    flip map (p ^. perNames) $ \n ->
        GatheredName
        { gatheredNamePrefix = n ^. nHonorificPrefix
        , gatheredNameFirstName = n ^. nGivenName
        , gatheredNameMiddleName = n ^. nMiddleName
        , gatheredNameLastName = n ^. nFamilyName
        , gatheredNameSuffix = n ^. nHonorificSuffix
        }

gatherEmails :: Person -> [Text]
gatherEmails p =
    nub $ flip mapMaybe (p ^. perEmailAddresses) $ \ea -> ea ^. eaValue

gatherPhoneNumbers :: Person -> [Text]
gatherPhoneNumbers p =
    nub $ flip mapMaybe (p ^. perPhoneNumbers) $ \pn -> pn ^. pnCanonicalForm
