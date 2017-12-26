{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.Git
    ( gitApplication
    ) where

import Import

import Control.Monad.Reader

import Network.HTTP.Types as Http
import Network.Wai as Wai
import Network.Wai.Application.CGI.Git
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.Rewrite

import Wolf.API
import Wolf.Data

import Wolf.Server
import Wolf.Server.Auth
import Wolf.Server.Path
import Wolf.Server.Types

import Wolf.Web.Server.Foundation

gitApplication :: WolfServerEnv -> Wai.Application
gitApplication wse req resp = do
    let authSets = "git"
    mdd <-
        case lookup hAuthorization (requestHeaders req) >>= extractBasicAuth of
            Nothing -> pure Nothing
            Just (key, pass) -> do
                maid <- basicAuthCheck wse key pass
                case maid of
                    Unauthorized -> pure Nothing
                    BadPassword -> pure Nothing
                    NoSuchUser -> pure Nothing
                    Authorized acc -> do
                        let aid = accountUUID acc
                        dd <- runReaderT (accountDataDir aid) wse
                        pure $ Just dd
    case mdd of
        Nothing -> authOnNoAuth authSets "git" req resp
        Just dd -> do
            let gitPath = toFilePath $ dd </> dotGit
            rewriteMiddleware (cgiGitBackend gitPath) req resp

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
