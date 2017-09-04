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
import Data.Time
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
        Just "person.names,person.emailAddresses,person.phoneNumbers"
    forM_ (resp ^. lcrConnections) $ \p -> do
        LB8.putStrLn $ JSON.encodePretty p
        LB8.putStrLn $ JSON.encodePretty $ gatherData now p

gatherData :: UTCTime -> Person -> Maybe GatheredPerson
gatherData now p = do
    let aliases = gatherAliases p
    let ns = gatherNames now p
    let ps = gatherEmails now p
    let pns = gatherPhoneNumbers now p
    pure
        GatheredPerson
        { gatheredPersonAliases = aliases
        , gatheredPersonEntry = PMap $ ns ++ ps ++ pns
        }

gatherAliases :: Person -> [Text]
gatherAliases p =
    nub $ flip mapMaybe (p ^. perNames) $ \n -> do
        firstName <- n ^. nGivenName
        lastName <- n ^. nFamilyName
        pure $ T.unwords [firstName, lastName]

val :: UTCTime -> Text -> PersonProperty
val now t =
    PVal
        PersonPropertyValue
        { personPropertyValueContents = t
        , personPropertyValueLastUpdatedTimestamp = now
        }

gatherNames :: UTCTime -> Person -> [(Text, PersonProperty)]
gatherNames now p =
    case ns of
        [] -> []
        [n] -> n
        ns_ -> [("name", PList $ map PMap ns_)]
  where
    ns =
        nub $ flip mapMaybe (p ^. perNames) $ \n -> do
            let el :: Text
                   -> Lens' Name (Maybe Text)
                   -> Maybe (Text, PersonProperty)
                el t mv = do
                    v <- n ^. mv
                    pure (t, val now v)
            pure $
                catMaybes
                    [ el "prefix" nHonorificPrefix
                    , el "first name" nGivenName
                    , el "middle name" nMiddleName
                    , el "last name" nFamilyName
                    , el "suffix" nHonorificSuffix
                    ]

gatherEmails :: UTCTime -> Person -> [(Text, PersonProperty)]
gatherEmails now p =
    case es of
        [] -> []
        [e] -> [("email", e)]
        _ -> [("email", PList es)]
  where
    es =
        nub $ flip mapMaybe (p ^. perEmailAddresses) $ \ea -> do
            ee <- ea ^. eaValue
            pure $ val now ee

gatherPhoneNumbers :: UTCTime -> Person -> [(Text, PersonProperty)]
gatherPhoneNumbers now p =
    case pns of
        [] -> []
        [pn] -> [("phone", pn)]
        _ -> [("phone", PList pns)]
  where
    pns =
        nub $ flip mapMaybe (p ^. perPhoneNumbers) $ \pn -> do
            v <- pn ^. pnCanonicalForm
            pure $ val now v

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
