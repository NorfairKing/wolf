{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.Draw where

import Import

import Data.Time

import Brick.AttrMap as A
import Brick.Types
import Brick.Util (fg)
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick.Widgets.List
import Graphics.Vty as V

import Wolf.Data
import Wolf.Data.Time

import Wolf.Cub.PropertyEditor
import Wolf.Cub.SearchBox
import Wolf.Cub.Types

drawUI :: CubState -> [Widget ResourceName]
drawUI CubState {..} =
    case cubStateShown of
        CubShowPersonList l -> drawPersonList l
        CubShowPerson ps -> drawPerson cubStateNow ps
        CubEditPerson eps -> drawEditPerson eps

drawPersonList :: PersonListState -> [Widget ResourceName]
drawPersonList PersonListState {..} =
    [helpUI | personListStateShowHelp] ++ [listUi]
  where
    listUi =
        borderWithLabel (txt "[Wolf Cub]") $
        renderList renderElement True personListStatePeopleList <=>
        case personListStateSearchBox of
            Nothing -> emptyWidget
            Just sb -> renderSearchBox sb
    renderElement :: Bool -> (Alias, PersonUuid) -> Widget ResourceName
    renderElement _ (name, _) = padLeftRight 1 $ txt $ aliasText name
    helpUI =
        centerLayer $
        borderWithLabel (txt "[Help]") $ vBox $ map txt ["q: Exit", "h: Help"]

drawPerson :: UTCTime -> PersonState -> [Widget ResourceName]
drawPerson now PersonState {..} = [helpUI | personStateShowHelp] ++ [personUI]
  where
    personUI =
        borderWithLabel (str $ "[" ++ uuidString personStateUuid ++ "]") $
        personEntryPart <=> hBorder <=> personNotesPart
    helpUI =
        centerLayer $
        borderWithLabel (txt "[Help]") $ vBox $ map txt ["q: Exit", "h: Help"]
    personEntryPart =
        case personStateEntry of
            Nothing ->
                let str_ = "No entry found."
                in str str_
            Just pe -> personEntryWidget now pe
    personNotesPart =
        vLimit 1 (center (withAttr headerAttr $ txt "Notes")) <=>
        renderList renderElement True personStateNotes
      where
        renderElement :: Bool -> (NoteUuid, Note) -> Widget ResourceName
        renderElement _ (_, Note {..}) =
            borderWithLabel
                (txt $ "[" <> formatMomentNicely now noteTimestamp <> "]") $
            txtWrap noteContents

personEntryWidget :: UTCTime -> PersonEntry -> Widget ResourceName
personEntryWidget now pe = go $ personEntryProperties pe
  where
    go (PVal PersonPropertyValue {..}) =
        hBox
            [ withAttr entryValueAttr (txt personPropertyValueContents)
            , txt "   ("
            , withAttr
                  entryLastUpdatedAttr
                  (txt (formatMomentNicely
                            now
                            personPropertyValueLastUpdatedTimestamp))
            , txt ")"
            ]
    go (PList vs) = vBox $ map ((txt "- " <+>) . go) vs
    go (PMap tups) =
        vBox $
        flip map tups $ \(k, v) ->
            let leftSide = withAttr entryKeyAttr (txt k) <+> txt ": "
            in case v of
                   (PVal _) -> leftSide <+> go v
                   _ -> leftSide <=> padLeft (Pad 2) (go v)

drawEditPerson :: EditPersonState -> [Widget ResourceName]
drawEditPerson eps = [editUi]
  where
    editUi =
        borderWithLabel (txt "[Edit]") $
        renderPropertyEditor $ editPersonStatePropertyEditor eps

headerAttr :: AttrName
headerAttr = "header"

entryKeyAttr :: AttrName
entryKeyAttr = "entry-key"

entryValueAttr :: AttrName
entryValueAttr = "entry-value"

entryLastUpdatedAttr :: AttrName
entryLastUpdatedAttr = "entry-last-updated"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (listSelectedAttr, fg V.brightWhite)
        , (headerAttr, fg V.white)
        , (entryKeyAttr, fg V.yellow)
        , (entryValueAttr, fg V.white)
        , (entryLastUpdatedAttr, currentAttr)
        , (propertyEditorAttr, fg V.white)
        , (propertyEditorAttrSelected, fg V.yellow)
        ]
