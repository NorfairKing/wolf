{-# LANGUAGE FlexibleContexts #-}

module Wolf where

import Import

import Wolf.Alias
import Wolf.Entry
import Wolf.Git
import Wolf.Init
import Wolf.Note
import Wolf.OptParse
import Wolf.Review
import Wolf.Summary

wolf :: IO ()
wolf = do
    (disp, sets) <- getInstructions
    runReaderT (dispatch disp) sets

dispatch :: (MonadIO m, MonadReader Settings m) => Dispatch -> m ()
dispatch DispatchInit = init
dispatch (DispatchNote person) = note person
dispatch (DispatchAlias new old) = alias new old
dispatch (DispatchSummary person) = summary person
dispatch (DispatchEntry person) = entry person
dispatch (DispatchGit cmds) = git cmds
dispatch DispatchReview = review
