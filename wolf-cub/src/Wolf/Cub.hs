{-# LANGUAGE RecordWildCards #-}

module Wolf.Cub where

import Import

import Wolf.Cub.OptParse

runWolfCub :: IO ()
runWolfCub = do
    (DispatchRun RunSettings {..}, Settings) <- getInstructions
    print runSetDataSettings
