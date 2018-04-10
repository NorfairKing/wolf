{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Export
    ( export
    ) where

import Import

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LB8

import Wolf.Data

import Wolf.Cli.OptParse
import Wolf.Cli.Utils

import Cautious.CautiousT

export :: (MonadIO m, MonadReader Settings m) => m ()
export =
    runData $
    withInitCheck_ $ do
        cautiousRepository <- runCautiousT exportRepo
        liftIO $
            case cautiousRepository of
                CautiousWarning [] repo -> LB8.putStrLn $ encodePretty repo
                CautiousWarning w repo -> do
                    LB8.putStrLn $ encodePretty repo
                    putStrLn $
                        "The warnings are:\n" ++ prettyShowExportWarning w
                CautiousError e ->
                    putStrLn $ "The errors are\n" ++ prettyShowExportError e
