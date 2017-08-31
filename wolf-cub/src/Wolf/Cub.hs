{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub where

import Import

import Control.Monad.Reader

import Data.Time
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

import Wolf.Data
import Wolf.Data.Time

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
    { cubStateShown = CubShowPersonList $ makePersonList i
    , cubStateNow = now
    , cubStateDataSettings = ds
    }

makePersonList :: Index -> List ResourceName (Text, PersonUuid)
makePersonList i = list "person-list" (V.fromList $ indexTuples i) 1

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
    case cubStateShown of
        CubShowPersonList l -> drawPersonList l
        CubShowPerson ps -> drawPerson cubStateNow ps

drawPersonList :: List ResourceName (Text, PersonUuid) -> [Widget ResourceName]
drawPersonList personList = [listUi]
  where
    listUi =
        borderWithLabel (txt "[Wolf Cub]") $
        renderList renderElement True personList
    renderElement :: Bool -> (Text, PersonUuid) -> Widget ResourceName
    renderElement _ (name, _) = padLeftRight 1 $ txt name

drawPerson :: UTCTime -> PersonState -> [Widget ResourceName]
drawPerson now PersonState {..} = [popup]
  where
    popup =
        borderWithLabel (str $ "[" ++ personUuidString personStateUuid ++ "]") $
        personEntryPart <=> hBorder <=> personNotesPart
    personEntryPart =
        case personStateEntry of
            Nothing ->
                let str_ = "No entry found."
                in str str_
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
                            (txt .
                             formatMomentNicely now .
                             personPropertyValueLastUpdatedTimestamp . snd)
                            tups
                in padLeftRight 1 $
                   keys <+> padLeftRight 1 values <+> lastChangeds
    personNotesPart =
        vLimit 1 (center (withAttr headerAttr $ txt "Notes")) <=>
        renderList renderElement True personStateNotes
      where
        renderElement :: Bool -> (NoteUuid, Note) -> Widget ResourceName
        renderElement _ (_, Note {..}) =
            borderWithLabel
                (txt $ "[" <> formatMomentNicely now noteTimestamp <> "]") $
            txtWrap noteContents

headerAttr :: AttrName
headerAttr = "header"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [(listSelectedAttr, fg V.brightWhite), (headerAttr, fg V.white)]

appEvent ::
       CubState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next CubState)
appEvent state e =
    case cubStateShown state of
        CubShowPersonList personList ->
            case e of
                (VtyEvent ve) ->
                    case ve of
                        (EvKey V.KEsc []) -> halt state
                        (EvKey (V.KChar 'q') []) -> halt state
                        (EvKey V.KEnter []) -> do
                            let msel = listSelectedElement personList
                            case msel of
                                Nothing -> continue state
                                Just (_, (_, personUuid)) -> do
                                    (mpe, ns) <-
                                        liftIO $
                                        flip
                                            runReaderT
                                            (cubStateDataSettings state) $ do
                                            mpe <- getPersonEntry personUuid
                                            nuuids <-
                                                getPersonNoteUuids personUuid
                                            ns <-
                                                fmap catMaybes $
                                                forM nuuids $ \uuid -> do
                                                    n <- readNote uuid
                                                    pure $ (,) uuid <$> n
                                            pure (mpe, ns)
                                    let nl = list "notes" (V.fromList ns) 1
                                    continue $
                                        state
                                        { cubStateShown =
                                              CubShowPerson
                                                  PersonState
                                                  { personStateUuid = personUuid
                                                  , personStateEntry = mpe
                                                  , personStateNotes = nl
                                                  }
                                        }
                        _ -> do
                            nl <- handleListEvent ve personList
                            continue $
                                state {cubStateShown = CubShowPersonList nl}
                _ -> continue state
        CubShowPerson person ->
            case e of
                (VtyEvent ve) ->
                    let unpop = do
                            index <-
                                runReaderT getIndexWithDefault $
                                cubStateDataSettings state
                            continue $
                                state
                                { cubStateShown =
                                      CubShowPersonList $ makePersonList index
                                }
                    in case ve of
                           (EvKey V.KEsc []) -> unpop
                           (EvKey (V.KChar 'q') []) -> unpop
                           _ -> do
                               nl <-
                                   handleListEvent ve $ personStateNotes person
                               continue $
                                   state
                                   { cubStateShown =
                                         CubShowPerson $
                                         person {personStateNotes = nl}
                                   }
                _ -> continue state
