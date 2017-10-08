{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli where

import Import

import Wolf.Cli.Command.Alias
import Wolf.Cli.Command.Entry
import Wolf.Cli.Command.Export
import Wolf.Cli.Command.Git
import Wolf.Cli.Command.Init
import Wolf.Cli.Command.Note
import Wolf.Cli.Command.RandomPerson
import Wolf.Cli.Command.Review
import Wolf.Cli.Command.Suggestion
import Wolf.Cli.Command.Summary
import Wolf.Cli.OptParse

wolf :: IO ()
wolf = do
    (disp, sets) <- getInstructions
    runReaderT (dispatch disp) sets

dispatch :: (MonadIO m, MonadReader Settings m) => Dispatch -> m ()
dispatch DispatchInit = init
dispatch (DispatchNote people) = note people
dispatch (DispatchAlias new old) = alias new old
dispatch (DispatchSummary person) = summary person
dispatch (DispatchEntry person) = entry person
dispatch (DispatchGit cmds) = git cmds
dispatch (DispatchReview pd) = review pd
dispatch DispatchRandomPerson = randomPerson
dispatch (DispatchSuggestion ss) = suggestion ss
dispatch DispatchExport = export
