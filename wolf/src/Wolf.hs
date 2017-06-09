{-# LANGUAGE TemplateHaskell #-}

module Wolf where

import Import

import Wolf.OptParse
import Wolf.Note

wolf :: IO ()
wolf = do
    (DispatchNote person, Settings) <- getInstructions
    note person
