{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.Git
    ( gitApplication
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Control.Monad.Reader

import System.Environment
import System.Process

import Data.Conduit.Binary (sourceHandle)
import Network.HTTP.Types as Http
import Network.Wai as Wai
import Network.Wai.Internal

import Yesod
import Yesod.Auth
import Yesod.Core.Handler

import Wolf.API
import Wolf.Data

import Wolf.Server.Path

import Wolf.Web.Server.Foundation

gitApplication :: ServerDataSettings -> Wai.Application
gitApplication sds req sendResp = do
    add <-
        case sds of
            PersonalServer ds -> pure $ dataSetWolfDir ds
            SharedServer wse -> do
                let aid =
                        fromJust $
                        parseAccountUUID "379cf27b-84c1-44bd-a73f-1f4315f5be9c"
                runReaderT (accountDataDir aid) wse
    e <- liftIO getEnvironment
    -- print $ ("Original path:", pathInfo req)
    let translatedPath =
            case pathInfo req of
                [] -> error "Should never happen." -- FIXME
                (_:rest) -> toFilePath add ++ T.unpack (T.intercalate "/" rest)
    -- print ("Translated path:", translatedPath)
    let hs = requestHeaders req
    let cp =
            (shell "git http-backend")
            { cwd = Just $ toFilePath add
            , env =
                  Just $
                  [ ("GIT_HTTP_EXPORT_ALL", "TRUE")
                  , ("PATH_TRANSLATED", translatedPath)
                  ] ++
                  maybeToList
                      (((,) "REMOTE_USER" . SB8.unpack) <$>
                       lookup "REMOTE_USER" (requestHeaders req)) ++
                  maybeToList
                      (((,) "REMOTE_ADDR" . SB8.unpack) <$>
                       requestHeaderHost req) ++
                  maybeToList
                      (((,) "CONTENT_TYPE" . SB8.unpack) <$>
                       lookup hContentType hs) ++
                  [ ("QUERY_STRING", SB8.unpack $ rawQueryString req)
                  , ("REQUEST_METHOD", SB8.unpack $ requestMethod req)
                  ] ++
                  e
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            }
    (minh, mouth, merrh, ph) <- liftIO $ createProcess cp
    case (,) <$> minh <*> mouth of
        Nothing -> error "something went wrong with git."
        Just (inh, outh) -> do
            liftIO $ lazyRequestBody req >>= LB.hPut inh
            let defResp = responseLBS status500 [] ""
            sendResp $
                responseRaw
                    (\getChunk sendChunk ->
                         let loop = do
                                 mec <- getProcessExitCode ph
                                 case mec of
                                     Just _ -> pure ()
                                     Nothing -> do
                                         d <- getChunk
                                         print "hi"
                                         SB.hPut inh d
                                         e <- SB.hGetNonBlocking outh 1024
                                         print "ho"
                                         sendChunk e
                                         loop
                         in loop)
                    defResp
