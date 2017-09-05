{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wolf.Google.Suggest where

import Import

import Control.Monad.Catch
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.List
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.TypeLits (Symbol)
import Lens.Micro
import Network.HTTP.Client (Manager)
import System.Exit (exitFailure)
import System.IO
import System.Info (os)
import System.Process (rawSystem)

import Network.Google
import Network.Google.Auth
import Network.Google.People

import Wolf.Google.Types

suggest :: IO ()
suggest = do
    lgr <- newLogger Debug stdout
    man <- newManager tlsManagerSettings
    store <- getUserAuth wolfOauthClient wolfScopes lgr man
    env <-
        newEnv <&> (envLogger .~ lgr) . (envScopes .~ wolfScopes) .
        (envStore .~ store)
    resp <-
        runResourceT . runGoogle env $ send $ peopleConnectionsList "people/me" &
        pclRequestMaskIncludeField .~
        Just "person.names,person.emailAddresses,person.phoneNumbers"
    forM_ (resp ^. lcrConnections) $ \p -> do
        LB8.putStrLn $ JSON.encodePretty p
        LB8.putStrLn $ JSON.encodePretty $ gatherData p

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
    nub $ flip mapMaybe (p ^. perNames) $ \n -> do
        firstName <- n ^. nGivenName
        lastName <- n ^. nFamilyName
        pure $ T.unwords [firstName, lastName]

gatherNames :: Person -> [GatheredName]
gatherNames p =
    nub $ flip map (p ^. perNames) $ \n ->
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

type WolfScopes = '[ "https://www.googleapis.com/auth/contacts.readonly"]

wolfScopes :: Proxy WolfScopes
wolfScopes = contactsReadOnlyScope

wolfOauthClient :: OAuthClient
wolfOauthClient =
    OAuthClient
    { _clientId =
          "130660029870-3e5q0l0nde157qkaehe5i3ib3tq730n7.apps.googleusercontent.com"
    , _clientSecret = "oqz9acOZo7JSSw3UUYGHmbFK"
    }

getUserAuth ::
       forall s proxy. AllowScopes (s :: [Symbol])
    => OAuthClient
    -> proxy s
    -> Logger
    -> Manager
    -> IO (Store s)
getUserAuth c proxy logger man = do
    creds <- fromWellKnownPath `catch` getCredsForTheFirstTime
    store <- initStore creds logger man
    auth <- retrieveAuthFromStore store
    case authToAuthorizedUser auth of
        Left err -> T.putStrLn err
        Right au -> saveAuthorizedUserToWellKnownPath True au
    pure store
  where
    getCredsForTheFirstTime :: AuthError -> IO (Credentials s)
    getCredsForTheFirstTime _ = do
        code <- redirectPrompt c proxy
        let creds = installedApplication c code
        pure creds

redirectPrompt ::
       AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> IO (OAuthCode s)
redirectPrompt c p = do
    let u = formURL c p
    T.putStrLn $ "Opening URL " `T.append` u
    _ <-
        case os of
            "darwin" -> rawSystem "open" [T.unpack u]
            "linux" -> rawSystem "xdg-open" [T.unpack u]
            _ -> T.putStrLn "Unsupported OS" >> exitFailure
    T.putStrLn "Please input the authorisation code: "
    OAuthCode <$> T.getLine
