{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Cub.Handle where

import Import

import Control.Monad.Reader

import qualified Data.Vector as V

import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Graphics.Vty as V

import Wolf.Data

import Wolf.Cub.Types

makePersonList :: Index -> List ResourceName (Text, PersonUuid)
makePersonList i = list "person-list" (V.fromList $ indexTuples i) 1

handleEvent ::
       CubState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next CubState)
handleEvent state e =
    case cubStateShown state of
        CubShowPersonList personList ->
            handleEventShowPersonList state e personList
        CubShowPerson person -> handleEventShowPerson state e person

handleEventShowPersonList ::
       CubState
    -> BrickEvent ResourceName ()
    -> PersonListState
    -> EventM ResourceName (Next CubState)
handleEventShowPersonList state e pls@PersonListState {..} =
    case e of
        (VtyEvent ve) ->
            if personListStateShowHelp
                then let unhelp =
                             continue $
                             state
                             { cubStateShown =
                                   CubShowPersonList $
                                   pls {personListStateShowHelp = False}
                             }
                     in case ve of
                            (EvKey V.KEsc []) -> unhelp
                            (EvKey (V.KChar 'q') []) -> unhelp
                            (EvKey (V.KChar 'h') []) -> unhelp
                            _ -> continue state
                else case ve of
                         (EvKey V.KEsc []) -> halt state
                         (EvKey (V.KChar 'q') []) -> halt state
                         (EvKey (V.KChar 'h') []) ->
                             continue
                                 state
                                 { cubStateShown =
                                       CubShowPersonList
                                           pls {personListStateShowHelp = True}
                                 }
                         (EvKey V.KEnter []) -> do
                             let msel =
                                     listSelectedElement personListStatePeople
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
                                                   { personStateUuid =
                                                         personUuid
                                                   , personStateEntry = mpe
                                                   , personStateNotes = nl
                                                   , personStateShowHelp = False
                                                   }
                                         }
                         _ -> do
                             nl <- handleListEvent ve personListStatePeople
                             let ns = pls {personListStatePeople = nl}
                             continue $
                                 state {cubStateShown = CubShowPersonList ns}
        _ -> continue state

handleEventShowPerson ::
       CubState
    -> BrickEvent ResourceName ()
    -> PersonState
    -> EventM ResourceName (Next CubState)
handleEventShowPerson state e person =
    case e of
        (VtyEvent ve) ->
            if personStateShowHelp person
                then let unhelp =
                             continue $
                             state
                             { cubStateShown =
                                   CubShowPerson $
                                   person {personStateShowHelp = False}
                             }
                     in case ve of
                            (EvKey V.KEsc []) -> unhelp
                            (EvKey (V.KChar 'q') []) -> unhelp
                            (EvKey (V.KChar 'h') []) -> unhelp
                            _ -> continue state
                else let unpop = do
                             index <-
                                 runReaderT getIndexWithDefault $
                                 cubStateDataSettings state
                             continue $
                                 state
                                 { cubStateShown =
                                       CubShowPersonList
                                           PersonListState
                                           { personListStatePeople =
                                                 makePersonList index
                                           , personListStateShowHelp = False
                                           }
                                 }
                     in case ve of
                            (EvKey V.KEsc []) -> unpop
                            (EvKey (V.KChar 'q') []) -> unpop
                            (EvKey (V.KChar 'h') []) ->
                                continue $
                                state
                                { cubStateShown =
                                      CubShowPerson $
                                      person {personStateShowHelp = True}
                                }
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
