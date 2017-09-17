{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Export
    ( export
    ) where

import Import

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LB8

import qualified Wolf.Data as Wolf

import Wolf.Cli.OptParse
import Wolf.Cli.Utils

export :: (MonadIO m, MonadReader Settings m) => m ()
export =
    runData $
    Wolf.withInitCheck_ $ do
        e <- Wolf.export
        liftIO . LB8.putStrLn $ encodePretty e
