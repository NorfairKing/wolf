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

import Wolf.Cub.PropertyEditor
import Wolf.Cub.SearchBox
import Wolf.Cub.Types

makePersonList :: Index -> List ResourceName (Alias, PersonUuid)
makePersonList i = list "person-list" (V.fromList $ indexTuples i) 1

handleEvent ::
     CubState
  -> BrickEvent ResourceName ()
  -> EventM ResourceName (Next CubState)
handleEvent state e =
  case cubStateShown state of
    CubShowPersonList pls -> handleEventShowPersonList state e pls
    CubShowPerson ps -> handleEventShowPerson state e ps
    CubEditPerson eps -> handleEditPerson state e eps

handleEventShowPersonList ::
     CubState
  -> BrickEvent ResourceName ()
  -> PersonListState
  -> EventM ResourceName (Next CubState)
handleEventShowPersonList state e pls@PersonListState {..} =
  case e of
    (VtyEvent ve) ->
      if personListStateShowHelp
        then let unhelp = setPersonListShowHelp state pls False
              in case ve of
                   (EvKey V.KEsc []) -> unhelp
                   (EvKey (V.KChar 'q') []) -> unhelp
                   (EvKey (V.KChar 'h') []) -> unhelp
                   _ -> continue state
        else case personListStateSearchBox of
               Nothing ->
                 case ve of
                   (EvKey V.KEsc []) -> halt state
                   (EvKey (V.KChar 'q') []) -> halt state
                   (EvKey (V.KChar 'h') []) ->
                     setPersonListShowHelp state pls True
                   (EvKey (V.KChar 's') []) -> showNewSearchBox state pls
                   (EvKey V.KEnter []) -> do
                     let msel = listSelectedElement personListStatePeopleList
                     case msel of
                       Nothing -> continue state
                       Just (_, (_, personUuid)) -> showPerson state personUuid
                   _ -> do
                     nl <- handleListEvent ve personListStatePeopleList
                     let ns = pls {personListStatePeopleList = nl}
                     continue $ state {cubStateShown = CubShowPersonList ns}
               Just sb ->
                 let unsearch =
                       continue $ setPersonListShowSearchBox state pls Nothing
                  in case ve of
                       (EvKey V.KEsc []) -> unsearch
                       (EvKey V.KEnter []) -> unsearch
                       _ -> do
                         let sb' = handleSearchBox sb ve
                         let pls' = pls {personListStateSearchBox = Just sb'}
                         let state' =
                               state {cubStateShown = CubShowPersonList pls'}
                         continue $
                           if sb == sb'
                             then state'
                             else refreshListFromSearch state' pls' sb'
    _ -> continue state

setPersonListShowHelp ::
     CubState -> PersonListState -> Bool -> EventM n (Next CubState)
setPersonListShowHelp state pls b =
  continue
    state {cubStateShown = CubShowPersonList pls {personListStateShowHelp = b}}

showNewSearchBox :: CubState -> PersonListState -> EventM n (Next CubState)
showNewSearchBox state pls@PersonListState {..} =
  continue $
  setPersonListShowSearchBox state pls $
  Just $
  searchBox "search-box" $ map (first aliasText) personListStateInitialPeople

setPersonListShowSearchBox ::
     CubState
  -> PersonListState
  -> Maybe (SearchBox ResourceName PersonUuid)
  -> CubState
setPersonListShowSearchBox state pls msb =
  state {cubStateShown = CubShowPersonList pls {personListStateSearchBox = msb}}

refreshListFromSearch ::
     CubState
  -> PersonListState
  -> SearchBox ResourceName PersonUuid
  -> CubState
refreshListFromSearch state pls sb =
  state
    { cubStateShown =
        CubShowPersonList
          pls
            { personListStatePeopleList =
                first alias <$>
                list
                  "person-list"
                  (V.fromList $ searchBoxCurrentlySelected sb)
                  1
            }
    }

handleEventShowPerson ::
     CubState
  -> BrickEvent ResourceName ()
  -> PersonState
  -> EventM ResourceName (Next CubState)
handleEventShowPerson state e ps@PersonState {..} =
  case e of
    (VtyEvent ve) ->
      if personStateShowHelp
        then let unhelp = setPersonShowHelp state ps False
              in case ve of
                   (EvKey V.KEsc []) -> unhelp
                   (EvKey (V.KChar 'q') []) -> unhelp
                   (EvKey (V.KChar 'h') []) -> unhelp
                   _ -> continue state
        else let unpop = showPersonList state
              in case ve of
                   (EvKey V.KEsc []) -> unpop
                   (EvKey (V.KChar 'q') []) -> unpop
                   (EvKey (V.KChar 'h') []) -> setPersonShowHelp state ps True
                   (EvKey (V.KChar 'e') []) -> editPerson state personStateUuid
                   _ -> do
                     nl <- handleListEvent ve personStateNotes
                     continue $
                       state
                         { cubStateShown =
                             CubShowPerson $ ps {personStateNotes = nl}
                         }
    _ -> continue state

setPersonShowHelp :: CubState -> PersonState -> Bool -> EventM n (Next CubState)
setPersonShowHelp state ps b =
  continue $
  state {cubStateShown = CubShowPerson $ ps {personStateShowHelp = b}}

handleEditPerson ::
     CubState
  -> BrickEvent ResourceName ()
  -> EditPersonState
  -> EventM ResourceName (Next CubState)
handleEditPerson state e eps@EditPersonState {..} =
  case e of
    (VtyEvent ve) ->
      case ve of
        (EvKey V.KEsc []) -> unpop
        (EvKey (V.KChar 'q') []) -> unpop
        _ -> do
          ne <- handlePropertyEditorEvent ve editPersonStatePropertyEditor
          continue $
            state
              { cubStateShown =
                  CubEditPerson $ eps {editPersonStatePropertyEditor = ne}
              }
    _ -> continue state
  where
    unpop = do
      save
      showPerson state editPersonStateUuid
    save =
      liftIO $
      flip runReaderT (cubStateDataSettings state) $
      case propertyEditorCurrentValue editPersonStatePropertyEditor of
        Nothing -> deletePersonEntry editPersonStateUuid
        Just pe -> putPersonEntry editPersonStateUuid pe

showPerson :: CubState -> PersonUuid -> EventM ResourceName (Next CubState)
showPerson state personUuid = do
  (mpe, ns) <-
    liftIO $
    flip runReaderT (cubStateDataSettings state) $ do
      mpe <- getPersonEntry personUuid
      nuuids <- getPersonNoteUuids personUuid
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
              , personStateShowHelp = False
              }
      }

showPersonList :: CubState -> EventM n (Next CubState)
showPersonList state = do
  index <- runReaderT getIndexWithDefault $ cubStateDataSettings state
  continue $
    state
      { cubStateShown =
          CubShowPersonList
            PersonListState
              { personListStateInitialPeople = indexTuples index
              , personListStatePeopleList = makePersonList index
              , personListStateShowHelp = False
              , personListStateSearchBox = Nothing
              }
      }

editPerson :: CubState -> PersonUuid -> EventM n (Next CubState)
editPerson state personUuid = do
  mpe <-
    liftIO $
    flip runReaderT (cubStateDataSettings state) $ getPersonEntry personUuid
  continue $
    state
      { cubStateShown =
          CubEditPerson
            EditPersonState
              { editPersonStateUuid = personUuid
              , editPersonStateStartingEntry = mpe
              , editPersonStatePropertyEditor =
                  propertyEditor "edit-person" $ personEntryProperties <$> mpe
              }
      }
