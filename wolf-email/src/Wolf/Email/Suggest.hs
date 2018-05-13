{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Email.Suggest
    ( wolfEmailSuggest
    ) where

import Import

import qualified Data.ByteString as SB
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import System.Exit

import Control.Monad.Reader

import Path

import qualified Codec.MIME.Parse as Email
import qualified Codec.MIME.Type as Email
import qualified Network.Mail.Parse.Parsers.HeaderFields as Email
import Network.Mail.Parse.Types (EmailAddress(..))

import Wolf.Data
import Wolf.Data.Baked (EmailAddressWithPurpose(..), fromEntry)

import Wolf.Email.OptParse
import Wolf.Email.Suggest.Types

wolfEmailSuggest :: (MonadIO m, MonadReader Settings m) => m ()
wolfEmailSuggest = do
    contents <- liftIO T.getContents
    let message = Email.parseMIMEMessage contents
    -- liftIO $ print message
    let headers = Email.mime_val_headers message
    -- liftIO $ mapM_ print headers
    let personParams =
            mapMaybe
                (\hn -> find ((== hn) . Email.paramName) headers)
                ["from", "to", "cc"]
    liftIO $ mapM_ print personParams
    let addresses =
            fmap concat $
            forM personParams $ Email.parseEmailAddressList . Email.paramValue
    liftIO $ do
        now <- getCurrentTime
        case addresses of
            Left err -> die $ T.unpack err
            Right as ->
                mapM_ print $
                flip mapMaybe as $ \a ->
                    case emailLabel a of
                        Nothing -> Nothing
                        Just label ->
                            Just
                                Suggestion
                                { suggestionSuggestor = "wolf-email"
                                , suggestionReason =
                                      "There was an email with this data"
                                , suggestionTimestamp = now
                                , suggestionData =
                                      EmailNameSuggestion
                                      { emailSuggestionName = label
                                      , emailSuggestionEmailAddress =
                                            emailAddress a
                                      }
                                }

emailNameSuggestionType :: SuggestionType
emailNameSuggestionType =
    SuggestionType {suggestionTypePath = $(mkRelDir "email-name")}
