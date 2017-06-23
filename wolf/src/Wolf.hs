{-# LANGUAGE FlexibleContexts #-}

module Wolf where

import Import

import Wolf.Command.Alias
import Wolf.Command.Entry
import Wolf.Command.Git
import Wolf.Command.Init
import Wolf.Command.Note
import Wolf.Command.Review
import Wolf.Command.Summary
import Wolf.OptParse

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
