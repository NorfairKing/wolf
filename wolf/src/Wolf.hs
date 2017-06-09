module Wolf where

import Import

import Wolf.Alias
import Wolf.Entry
import Wolf.Git
import Wolf.Note
import Wolf.OptParse
import Wolf.Summary

wolf :: IO ()
wolf = do
    (disp, Settings) <- getInstructions
    dispatch disp

dispatch :: Dispatch -> IO ()
dispatch (DispatchNote person) = note person
dispatch (DispatchAlias new old) = alias new old
dispatch (DispatchSummary person) = summary person
dispatch (DispatchEntry person) = entry person
dispatch (DispatchGit cmds) = git cmds
