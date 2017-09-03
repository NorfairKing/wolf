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

import Safe

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
            let modSel func =
                    pure
                        pe
                        { propertyEditorSelection =
                              func propertyEditorSelection prop
                        }
            in case e of
                   (EvKey KDown []) -> modSel selectionDown
                   (EvKey KUp []) -> modSel selectionUp
                   (EvKey KLeft []) -> modSel selectionLeft
                   (EvKey KRight []) -> modSel selectionRight
                   _ -> pure pe

data P a
    = PV a
    | PL [P a]

toP :: PersonProperty -> P PersonPropertyValue
toP (PVal a) = PV a
toP (PList ls) = PL $ map toP ls
toP (PMap ls) = PL $ map (toP . snd) ls

-- Try to select a subtree of a 'P'
--
-- Returns 'Nothing' if the selection is invalid and 'Just' with the selection
-- if the selection is valid.
select :: Maybe [Int] -> P a -> Maybe (P a)
select Nothing _ = Nothing
select (Just []) p = Just p
select (Just _) (PV _) = Nothing
select (Just (i:is)) (PL ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just p -> select (Just is) p

unsnocMay :: [a] -> Maybe ([a], a)
unsnocMay as = (,) <$> initMay as <*> lastMay as

makeNewSel ::
       (([Int], Int) -> [Int]) -> Maybe [Int] -> PersonProperty -> Maybe [Int]
makeNewSel func msel prop =
    case msel of
        Nothing -> Just [0]
        Just sel ->
            let newSel = func <$> unsnocMay sel
            in case select newSel (toP prop) of
                   Nothing -> msel
                   Just _ -> newSel

selectionUp :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionUp = makeNewSel $ \(is, i) -> is ++ [i - 1]

selectionDown :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionDown = makeNewSel $ \(is, i) -> is ++ [i + 1]

selectionLeft :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionLeft = makeNewSel fst

selectionRight :: Maybe [Int] -> PersonProperty -> Maybe [Int]
selectionRight = makeNewSel $ \(is, i) -> is ++ [i, 0]
