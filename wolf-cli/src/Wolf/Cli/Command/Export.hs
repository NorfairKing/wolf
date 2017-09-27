{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Export
    ( export
    ) where

import Import

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LB8

import Wolf.Data

import Wolf.Cli.OptParse
import Wolf.Cli.Utils

export :: (MonadIO m, MonadReader Settings m) => m ()
export =
    runData $
    withInitCheck_ $ do
        e <- exportRepo
        liftIO . LB8.putStrLn $ encodePretty e
