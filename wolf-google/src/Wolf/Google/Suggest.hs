{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wolf.Google.Suggest where

import Import

import Control.Monad.Catch
import Data.List
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
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

import Wolf.Data

import Wolf.Google.Types

suggest :: IO ()
suggest = do
    now <- getCurrentTime
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
        LB8.putStrLn $ JSON.encodePretty $ gatherData now p

gatherData :: UTCTime -> Person -> Maybe GatheredPerson
gatherData now p = do
    let aliases = gatherAliases p
    let val t =
            PVal $
            PersonPropertyValue
            { personPropertyValueContents = t
            , personPropertyValueLastUpdatedTimestamp = now
            }
    let ns = gatherNames now p
    let ps =
            catMaybes
                [ case p ^. perEmailAddresses of
                      [] -> Nothing
                      [ea] -> do
                          v <- ea ^. eaValue
                          pure ("email", val v)
                      eas ->
                          Just
                              ( "email"
                              , PList $ flip mapMaybe eas $ \ea -> do
                                    v <- ea ^. eaValue
                                    pure $ val v)
                ]
    pure
        GatheredPerson
        {gatheredPersonAliases = aliases, gatheredPersonEntry = PMap $ ns ++ ps}

gatherNames :: UTCTime -> Person -> [(Text, PersonProperty)]
gatherNames now p =
    case ns of
        [] -> []
        [n] -> n
        ns -> [("name", PList $ map PMap ns)]
  where
    val t =
        PVal $
        PersonPropertyValue
        { personPropertyValueContents = t
        , personPropertyValueLastUpdatedTimestamp = now
        }
    ns =
        nub $ flip mapMaybe (p ^. perNames) $ \n -> do
            let el :: Text
                   -> Lens' Name (Maybe Text)
                   -> Maybe (Text, PersonProperty)
                el t mv = do
                    v <- n ^. mv
                    pure (t, val v)
            pure $
                catMaybes
                    [el "first name" nGivenName, el "last name" nFamilyName]

gatherAliases :: Person -> [Text]
gatherAliases p =
    flip mapMaybe (p ^. perNames) $ \n -> do
        firstName <- n ^. nGivenName
        lastName <- n ^. nFamilyName
        pure $ T.unwords [firstName, lastName]

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
