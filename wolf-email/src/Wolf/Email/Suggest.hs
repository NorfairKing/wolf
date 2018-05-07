{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Email.Suggest
    ( wolfEmailSuggest
    ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Control.Monad.Reader

import Text.Email.Validate as EmailAddress (toByteString)

import Wolf.Data
import Wolf.Data.Baked (EmailAddressWithPurpose(..), fromEntry)

import Wolf.Email.OptParse
import Wolf.Email.Suggest.Types

wolfEmailSuggest :: (MonadIO m, MonadReader Settings m) => m ()
wolfEmailSuggest = pure ()
