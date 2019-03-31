{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wolf.Google.Suggest.Call
  ( getPeople
  ) where

import Import

import Control.Monad.Catch
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

getPeople :: IO [ListConnectionsResponse]
getPeople = do
  lgr <- newLogger Debug stdout
  man <- newManager tlsManagerSettings
  store <- getUserAuth wolfOauthClient wolfScopes lgr man
  env <-
    newEnv <&> (envLogger .~ lgr) . (envScopes .~ wolfScopes) .
    (envStore .~ store)
  runResourceT . runGoogle env $ makeCalls

makeCalls :: Google WolfScopes [ListConnectionsResponse]
makeCalls = do
  let origReq :: PeopleConnectionsList
      origReq =
        peopleConnectionsList "people/me" & pclRequestMaskIncludeField .~
        Just "person.names,person.emailAddresses,person.phoneNumbers"
  resp <- send origReq
  let go r =
        case r ^. lcrNextSyncToken of
          Nothing -> pure []
          Just npt -> do
            r' <- send $ origReq & pclPageToken .~ Just npt
            rest <- go r'
            pure $ r' : rest
  rest <- go resp
  pure (resp : rest)

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
