{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Cub.PropertyEditor
    ( PropertyEditor
    , propertyEditor
    , renderPropertyEditor
    , handlePropertyEditorEvent
    , propertyEditorAttr
    , propertyEditorAttrSelected
    ) where

import Import

import Graphics.Vty as Vty
import Graphics.Vty as V

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core

import Wolf.Data

data PropertyEditor n = PropertyEditor
    { propertyEditorName :: n
    , propertyEditorProperty :: Maybe PersonProperty
    , propertyEditorSelection :: Maybe [Int]
    } deriving (Show, Generic)

propertyEditor :: n -> Maybe PersonProperty -> PropertyEditor n
propertyEditor name mprop =
    PropertyEditor
    { propertyEditorName = name
    , propertyEditorProperty = mprop
    , propertyEditorSelection = Nothing
    }

renderPropertyEditor :: PropertyEditor n -> Widget n
renderPropertyEditor PropertyEditor {..} =
    withAttr propertyEditorAttr $
    case propertyEditorProperty of
        Nothing -> txt "No properties, press 's' to start a new property."
        Just pp -> padRight Max $ padBottom Max $ go propertyEditorSelection pp
  where
    go :: Maybe [Int] -> PersonProperty -> Widget n
    go msel (PVal PersonPropertyValue {..}) =
        withSelectedAttr msel $ txt personPropertyValueContents
    go msel (PList vs) =
        withSelectedAttr msel $
        vBox $
        map (txt "- " <+>) $
        flip map (zip [0 ..] vs) $ \(ix, v) ->
            let msel' = drillSel msel ix
            in go msel' v
    go msel (PMap tups) =
        withSelectedAttr msel $
        vBox $
        flip map (zip [0 ..] tups) $ \(ix, (k, v)) ->
            let msel' = drillSel msel ix
                leftSide = withSelectedAttr msel' $ txt k <+> txt ": "
            in case v of
                   (PVal _) -> leftSide <+> go msel' v
                   _ -> leftSide <=> padLeft (Pad 2) (go msel' v)
    drillSel msel ix =
        case msel of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (x:xs) ->
                if x == ix
                    then Just xs
                    else Nothing
    withSelectedAttr msel =
        case msel of
            Nothing -> id
            Just [] -> withAttr propertyEditorAttrSelected
            Just _ -> id

propertyEditorAttr :: AttrName
propertyEditorAttr = "property-editor"

propertyEditorAttrSelected :: AttrName
propertyEditorAttrSelected = propertyEditorAttr <> "property-editor-selected"

handlePropertyEditorEvent ::
       Eq n => Vty.Event -> PropertyEditor n -> EventM n (PropertyEditor n)
handlePropertyEditorEvent e pe@PropertyEditor {..} =
    case propertyEditorProperty of
        Nothing ->
            case e of
                (EvKey (V.KChar 's') []) ->
                    pure pe {propertyEditorProperty = Just $ PMap []}
                _ -> pure pe
        Just prop ->
            case e of
                (EvKey KDown []) ->
                    pure
                        pe
                        { propertyEditorSelection =
                              selectionDown propertyEditorSelection prop
                        }
                (EvKey KUp []) ->
                    pure
                        pe
                        { propertyEditorSelection =
                              selectionUp propertyEditorSelection prop
                        }
                _ -> pure pe

selectionUp :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionUp Nothing _ = Just [0]
selectionUp (Just [i]) prop =
    case prop of
        PVal _ -> Just [i]
        PList _ ->
            if i > 0
                then Just [i - 1]
                else Just [i]
        PMap _ ->
            if i > 0
                then Just [i - 1]
                else Just [i]
selectionUp msel _ = msel

selectionDown :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionDown Nothing _ = Just [0]
selectionDown (Just [i]) prop =
    case prop of
        PVal _ -> Just [i]
        PList ps ->
            if i + 1 < length ps
                then Just [i + 1]
                else Just [i]
        PMap ps ->
            if i + 1 < length ps
                then Just [i + 1]
                else Just [i]
selectionDown msel _ = msel
