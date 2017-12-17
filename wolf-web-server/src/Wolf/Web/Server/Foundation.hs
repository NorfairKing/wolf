{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Web.Server.Foundation
    ( module Wolf.Web.Server.Foundation
    , module Wolf.Web.Server.Widget
    , module Wolf.Web.Server.Constants
    ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import qualified Network.HTTP.Client as Http
import Text.Hamlet
import Yesod
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg

import Wolf.API
import Wolf.Data

import Wolf.Server.Accounts
import Wolf.Server.Path
import Wolf.Server.Types hiding (WolfHandler)

import Wolf.Web.Server.Constants
import Wolf.Web.Server.Widget


type WolfWidget = WolfWidget' ()

type WolfWidget' = WidgetT App IO

type WolfHandler = HandlerT App IO

type WolfAuthHandler = HandlerT Auth WolfHandler

data ServerDataSettings
    = PersonalServer DataSettings
    | SharedServer WolfServerEnv

data App = App
    { appDataSettings :: ServerDataSettings
    , appHttpManager :: Http.Manager
    , appGit :: WaiSubsite
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent $(widgetFile "default-body")
        withUrlRenderer $(hamletFile "templates/default-page.hamlet")
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    authRoute _ = Just $ AuthR LoginR

instance YesodAuth App where
    type AuthId App = AccountUUID
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authHttpManager = appHttpManager
    authenticate creds = do
        ds <- asks appDataSettings
        case ds of
            PersonalServer _ -> pure $ Authenticated undefined
            SharedServer senv ->
                if credsPlugin creds == wolfAuthPluginName
                    then case username $ credsIdent creds of
                             Nothing ->
                                 pure $ UserError Msg.NoIdentifierProvided
                             Just un -> do
                                 mec <-
                                     flip runReaderT senv $ lookupAccountUUID un
                                 pure $
                                     case mec of
                                         Nothing ->
                                             UserError $
                                             Msg.IdentifierNotFound $
                                             usernameText un
                                         Just uuid -> Authenticated uuid
                    else pure $
                         ServerError $
                         T.unwords
                             [ "Unknown authentication plugin:"
                             , credsPlugin creds
                             ]
    authPlugins _ = [wolfAuthPlugin]
    maybeAuthId =
        runMaybeT $ do
            s <- MaybeT $ lookupSession credsKey
            MaybeT $ return $ fromPathPiece s

wolfAuthPluginName :: Text
wolfAuthPluginName = "wolf-auth-plugin"

wolfAuthPlugin :: AuthPlugin App
wolfAuthPlugin = AuthPlugin wolfAuthPluginName dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["register"] = getNewAccountR >>= sendResponse
    dispatch "POST" ["register"] = postNewAccountR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> WolfWidget
    loginWidget _ = do
        token <- genToken
        $(widgetFile "auth/login")

loginR :: AuthRoute
loginR = PluginR wolfAuthPluginName ["login"]

data LoginData = LoginData
    { loginUserkey :: Text
    , loginPassword :: Text
    } deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR wolfAuthPluginName ["login"]

postLoginR :: WolfAuthHandler TypedContent
postLoginR = do
    ds <- lift $ asks appDataSettings
    case ds of
        PersonalServer _ -> notFound
        SharedServer senv -> do
            let loginInputForm =
                    LoginData <$> ireq textField "userkey" <*>
                    ireq passwordField "passphrase"
            result <- lift $ runInputPostResult loginInputForm
            muser <-
                case result of
                    FormMissing -> invalidArgs ["Form is missing"]
                    FormFailure _ -> return $ Left Msg.InvalidLogin
                    FormSuccess (LoginData ukey pwd) ->
                        case username ukey of
                            Nothing -> pure $ Left Msg.InvalidUsernamePass
                            Just un -> do
                                mu <-
                                    flip runReaderT senv $ lookupAccountUUID un
                                case mu of
                                    Nothing ->
                                        return $ Left Msg.InvalidUsernamePass
                                    Just uuid -> do
                                        macc <-
                                            flip runReaderT senv $
                                            getAccount uuid
                                        case macc of
                                            Nothing ->
                                                return $
                                                Left Msg.InvalidUsernamePass
                                            Just acc ->
                                                return $
                                                if validatePassword
                                                       (accountPasswordHash acc)
                                                       (TE.encodeUtf8 pwd)
                                                    then Right acc
                                                    else Left
                                                             Msg.InvalidUsernamePass
            case muser of
                Left err -> loginErrorMessageI LoginR err
                Right acc ->
                    lift $
                    setCredsRedirect $
                    Creds
                        wolfAuthPluginName
                        (usernameText $ accountUsername acc)
                        []

registerR :: AuthRoute
registerR = PluginR wolfAuthPluginName ["register"]

getNewAccountR :: WolfAuthHandler Html
getNewAccountR = do
    token <- genToken
    lift $ defaultLayout $(widgetFile "auth/register")

data NewAccount = NewAccount
    { newAccountUsername :: Username
    , newAccountPassword1 :: Text
    , newAccountPassword2 :: Text
    } deriving (Show)

postNewAccountR :: WolfAuthHandler Html
postNewAccountR = do
    ds <- lift $ asks appDataSettings
    case ds of
        PersonalServer _ -> notFound
        SharedServer senv -> do
            let newAccountInputForm =
                    NewAccount <$>
                    ireq
                        (checkMMap
                             (\t ->
                                  pure $
                                  case username t of
                                      Nothing ->
                                          Left ("Invalid username" :: Text)
                                      Just un -> Right un)
                             usernameText
                             textField)
                        "username" <*>
                    ireq passwordField "passphrase" <*>
                    ireq passwordField "passphrase-confirm"
            mr <- lift getMessageRender
            result <- lift $ runInputPostResult newAccountInputForm
            mdata <-
                case result of
                    FormMissing -> invalidArgs ["Form is missing"]
                    FormFailure msg -> return $ Left msg
                    FormSuccess d ->
                        return $
                        if newAccountPassword1 d == newAccountPassword2 d
                            then Right
                                     Register
                                     { registerUsername = newAccountUsername d
                                     , registerPassword = newAccountPassword1 d
                                     }
                            else Left [mr Msg.PassMismatch]
            case mdata of
                Left errs -> do
                    setMessage $ toHtml $ T.concat errs
                    redirect registerR
                Right reg -> do
                    errOrUuid <- flip runReaderT senv $ registerAccount reg
                    case errOrUuid of
                        Left _ -> redirect registerR
                        Right _ -> redirect LoginR

runDataApp :: App -> ReaderT DataSettings IO a -> Handler a
runDataApp app func = do
    let sds = appDataSettings app
    case sds of
        PersonalServer ds -> liftIO $ runReaderT func ds
        SharedServer wse -> do
            uuid <- requireAuthId
            dd <- runReaderT (accountDataDir uuid) wse
            let ds = DataSettings {dataSetWolfDir = dd}
            liftIO $ runReaderT func ds

runData :: ReaderT DataSettings IO a -> Handler a
runData func = do
    app <- getYesod
    runDataApp app func

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance PathPiece AccountUUID where
    fromPathPiece = parseAccountUUID
    toPathPiece = accountUUIDText

instance PathPiece PersonUuid where
    fromPathPiece = parsePersonUuid
    toPathPiece = personUuidText

withNavBar :: WidgetT App IO () -> WidgetT App IO ()
withNavBar widget = $(widgetFile "with-nav-bar")

genToken :: MonadHandler m => m Html
genToken = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    pure $
        case reqToken req of
            Nothing -> mempty
            Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

isMultiUser :: Handler Bool
isMultiUser = do
    sds <- asks appDataSettings
    pure $
        case sds of
            PersonalServer _ -> False
            SharedServer _ -> True

aliasField :: Field Handler Alias
aliasField =
    checkMMap (pure . (Right :: a -> Either Text a) . alias) aliasText textField
