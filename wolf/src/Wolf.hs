module Wolf where

import Import

import Wolf.Note
import Wolf.OptParse
import Wolf.Summary

wolf :: IO ()
wolf = do
    (disp, Settings) <- getInstructions
    dispatch disp

dispatch :: Dispatch -> IO ()
dispatch (DispatchNote person) = note person
dispatch (DispatchSummary person) = summary person
