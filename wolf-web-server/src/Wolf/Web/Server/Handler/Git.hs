{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.Git
    ( gitApplication
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Text as T

import Control.Monad.Reader

import System.Environment
import System.Posix.Signals
import System.Process

import Data.Conduit.Binary (sourceHandle)
import Network.HTTP.Types as Http
import Network.Wai as Wai
import Network.Wai.Application.CGI.Git
import Network.Wai.Internal
import Network.Wai.Middleware.Rewrite

import Yesod
import Yesod.Auth
import Yesod.Core.Handler

import Wolf.API
import Wolf.Data

import Wolf.Server.Path

import Wolf.Web.Server.Foundation

gitApplication :: ServerDataSettings -> Wai.Application
gitApplication sds req respond = do
    dd <-
        case sds of
            PersonalServer ds -> pure $ dataSetWolfDir ds
            SharedServer wse -> do
                let aid =
                        fromJust $
                        parseAccountUUID "379cf27b-84c1-44bd-a73f-1f4315f5be9c"
                runReaderT (accountDataDir aid) wse
    let gitPath = toFilePath $ dd </> dotGit
    -- print gitPath
    rewriteMiddleware (cgiGitBackend gitPath) req respond

dotGit :: Path Rel Dir
dotGit = $(mkRelDir ".git")

-- We want to rewrite `/git` to `/`.
rewriteMiddleware :: Middleware
rewriteMiddleware =
    rewritePureWithQueries $ \(ps, q) _ ->
        ( case ps of
              ("git":rest) -> rest
              _ -> ps
        , q)
