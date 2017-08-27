{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.RandomPerson
    ( randomPerson
    ) where

import Import

import qualified Data.Map as M

import System.Random

import Wolf.Cli.Command.Summary
import Wolf.Cli.OptParse
import Wolf.Cli.Utils
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Types

randomPerson :: (MonadIO m, MonadReader Settings m) => m ()
randomPerson =
    runData $
    withInitCheck $ do
        index <- getIndexWithDefault
        let es = M.elems $ indexMap index
        el <- liftIO $ randomRIO (0, length es - 1)
        let chosenPerson = es !! el
        printSummaryReportFor chosenPerson
