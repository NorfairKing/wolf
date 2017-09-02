{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub where

import Import

import Control.Monad.Reader

import Data.Time

import Brick.Main

import Wolf.Data

import Wolf.Cub.Draw
import Wolf.Cub.Handle
import Wolf.Cub.OptParse
import Wolf.Cub.Types

runWolfCub :: IO ()
runWolfCub = do
    (DispatchRun RunSettings {..}, Settings) <- getInstructions
    index <- runReaderT getIndexWithDefault runSetDataSettings
    now <- getCurrentTime
    void $
        Brick.Main.defaultMain cubApp $
        initialState now index runSetDataSettings

initialState :: UTCTime -> Index -> DataSettings -> CubState
initialState now i ds =
    CubState
    { cubStateShown =
          CubShowPersonList
              PersonListState
              { personListStatePeople = makePersonList i
              , personListStateShowHelp = False
              }
    , cubStateNow = now
    , cubStateDataSettings = ds
    }

cubApp :: App CubState () ResourceName
cubApp =
    App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }
