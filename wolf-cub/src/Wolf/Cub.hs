{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub where

import Import

import Control.Monad.Reader

import qualified Data.Vector as V

import Brick.AttrMap as A
import Brick.Main
import Brick.Types
import Brick.Util (fg)
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick.Widgets.List
import Graphics.Vty as V

import Wolf.Data.Entry
import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.Cub.OptParse
import Wolf.Cub.Types

runWolfCub :: IO ()
runWolfCub = do
    (DispatchRun RunSettings {..}, Settings) <- getInstructions
    index <- runReaderT getIndexWithDefault runSetDataSettings
    void $ Brick.Main.defaultMain cubApp $ initialState index runSetDataSettings

initialState :: Index -> DataSettings -> CubState
initialState i ds =
    CubState
    { cubStateShown =
          CubShown
          { cubShownPersonList =
                list "person-list" (V.fromList $ indexTuples i) 1
          , cubShownPopupUuid = Nothing
          }
    , cubStateDataSettings = ds
    }

cubApp :: App CubState () ResourceName
cubApp =
    App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

drawUI :: CubState -> [Widget ResourceName]
drawUI CubState {..} =
    (case cubShownPopupUuid of
         Nothing -> []
         Just personUuid -> [popup personUuid]) ++
    [listUi]
  where
    CubShown {..} = cubStateShown
    listUi =
        borderWithLabel (txt "[Wolf Cub]") $
        renderList renderElement True cubShownPersonList
    renderElement :: Bool -> (Text, PersonUuid) -> Widget ResourceName
    renderElement _ (name, _) = padLeftRight 1 $ txt name
    popup (personUuid, mpe) =
        centerLayer $
        borderWithLabel (str $ personUuidString personUuid) $
        padAll 1 $
        case mpe of
            Nothing ->
                let str_ = "No entry found."
                in vLimit 3 $ hLimit (length str_ + 2) $ str str_
            Just pe ->
                let tups = personEntryTuples pe
                    keys =
                        vBox $
                        withAttr headerAttr (txt "Key") :
                        map (txt . (<> ":") . fst) tups
                    values =
                        vBox $
                        withAttr headerAttr (txt "Value") :
                        map (txt . personPropertyValueContents . snd) tups
                    lastChangeds =
                        vBox $
                        withAttr headerAttr (txt "Last Updated") :
                        map
                            (str .
                             show .
                             personPropertyValueLastUpdatedTimestamp . snd)
                            tups
                in keys <+> padLeftRight 1 values <+> lastChangeds

headerAttr :: AttrName
headerAttr = "header"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (listAttr, fg V.white)
        , (listSelectedAttr, fg V.blue)
        , (headerAttr, fg V.brightWhite)
        ]

appEvent ::
       CubState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next CubState)
appEvent state e =
    let cs@CubShown {..} = cubStateShown state
    in do nshown <-
              case e of
                  (VtyEvent ve) ->
                      case cubShownPopupUuid of
                          Nothing ->
                              case ve of
                                  (EvKey V.KEsc []) -> halt cs
                                  (EvKey (V.KChar 'q') []) -> halt cs
                                  (EvKey V.KEnter []) -> do
                                      let msel =
                                              listSelectedElement
                                                  cubShownPersonList
                                      case msel of
                                          Nothing -> continue cs
                                          Just (_, (_, personUuid)) -> do
                                              mpe <-
                                                  liftIO $
                                                  runReaderT
                                                      (getPersonEntry personUuid) $
                                                  cubStateDataSettings state
                                              continue $
                                                  cs
                                                  { cubShownPopupUuid =
                                                        Just (personUuid, mpe)
                                                  }
                                  _ -> do
                                      nl <-
                                          handleListEvent ve cubShownPersonList
                                      continue $ cs {cubShownPersonList = nl}
                          Just _ ->
                              let unpop =
                                      continue $
                                      cs {cubShownPopupUuid = Nothing}
                              in case ve of
                                     (EvKey V.KEsc []) -> unpop
                                     (EvKey (V.KChar 'q') []) -> unpop
                                     _ -> continue cs
                  _ -> continue cs
          pure $ (\shown -> state {cubStateShown = shown}) <$> nshown
