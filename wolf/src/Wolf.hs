module Wolf where

import Import

import Wolf.Note
import Wolf.OptParse

wolf :: IO ()
wolf = do
    (DispatchNote person, Settings) <- getInstructions
    note person
