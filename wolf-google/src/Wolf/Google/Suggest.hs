{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wolf.Google.Suggest where

import Import

import Control.Monad.Catch
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
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
        Just "person.names,person.emailAddresses"
    forM_ (resp ^. lcrConnections) $ \p -> do
        LB8.putStrLn $ JSON.encodePretty p
        LB8.putStrLn $ JSON.encodePretty $ makeSuggestion p

makeSuggestion :: Person -> Maybe PersonSuggestion
makeSuggestion p =
    case p ^. perNames of
        [] -> Nothing
        (n:_) -> do
            firstName <- n ^. nGivenName
            lastName <- n ^. nFamilyName
            let ps =
                    catMaybes
                        [ case p ^. perEmailAddresses of
                              [] -> Nothing
                              [ea] -> do
                                  v <- ea ^. eaValue
                                  pure ("email", JSON.String v)
                              eas ->
                                  Just
                                      ( "email"
                                      , JSON.Array $ V.fromList $
                                        flip mapMaybe eas $ \ea -> do
                                            v <- ea ^. eaValue
                                            pure $ JSON.String v)
                        ]
            pure
                PersonSuggestion
                { personSuggestionAlias = T.unwords [firstName, lastName]
                , personSuggestionEntry =
                      JSON.object $
                      [ ("first name", JSON.String firstName)
                      , ("last name", JSON.String lastName)
                      ] ++
                      ps
                }

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
