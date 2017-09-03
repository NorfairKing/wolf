{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import qualified Data.Text as T

import Safe

import Graphics.Vty as Vty
import Graphics.Vty as V

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit

import Wolf.Data

data PropertyEditor n = PropertyEditor
    { propertyEditorName :: n
    , propertyEditorProperty :: Maybe PersonProperty
    , propertyEditorSelection :: Maybe [Int]
    , propertyEditorCurrentEditor :: Maybe (Editor Text n)
    } deriving (Show, Generic)

propertyEditor :: n -> Maybe PersonProperty -> PropertyEditor n
propertyEditor name mprop =
    PropertyEditor
    { propertyEditorName = name
    , propertyEditorProperty = mprop
    , propertyEditorSelection = Nothing
    , propertyEditorCurrentEditor = Nothing
    }

renderPropertyEditor :: (Show n, Ord n) => PropertyEditor n -> Widget n
renderPropertyEditor PropertyEditor {..} =
    withAttr propertyEditorAttr $
    vBox
        [ case propertyEditorProperty of
              Nothing -> txt "No properties, press 's' to start a new property."
              Just pp ->
                  padRight Max $ padBottom Max $ go propertyEditorSelection pp
        , case propertyEditorCurrentEditor of
              Nothing -> emptyWidget
              Just e -> renderEditor (txt . T.concat) True e
        ]
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

emptyProperty :: PersonProperty
emptyProperty = PMap []

handlePropertyEditorEvent ::
       (Monoid n, Eq n)
    => Vty.Event
    -> PropertyEditor n
    -> EventM n (PropertyEditor n)
handlePropertyEditorEvent e pe@PropertyEditor {..} =
    case propertyEditorProperty of
        Nothing ->
            case e of
                (EvKey (V.KChar 's') []) ->
                    pure pe {propertyEditorProperty = Just emptyProperty}
                _ -> pure pe
        Just prop ->
            case propertyEditorCurrentEditor of
                Nothing ->
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
                           (EvKey (KChar 'e') []) ->
                               case select propertyEditorSelection prop of
                                   Nothing -> pure pe -- Do nothing if the selection is invalid.
                                   Just (PVal ppv) ->
                                       pure $
                                       pe
                                       { propertyEditorCurrentEditor =
                                             Just $
                                             editorText
                                                 (propertyEditorName <>
                                                  propertyEditorName -- Weird hack to get the name to be unique.
                                                  )
                                                 (Just 1)
                                                 (personPropertyValueContents
                                                      ppv)
                                       }
                                   Just _ -> pure pe -- Do nothing if the selection is not specific.
                           _ -> pure pe
                Just ed ->
                    case e of
                        (EvKey KEnter []) ->
                            pure $ pe {propertyEditorCurrentEditor = Nothing}
                        _ -> do
                            ne <- handleEditorEvent e ed
                            pure $ pe {propertyEditorCurrentEditor = Just ne}

-- Try to select a subtree of a 'PersonProperty'
--
-- Returns 'Nothing' if the selection is invalid and 'Just' with the selection
-- if the selection is valid.
select :: Maybe [Int] -> PersonProperty -> Maybe PersonProperty
select Nothing _ = Nothing
select (Just []) p = Just p
select (Just _) (PVal _) = Nothing
select (Just (i:is)) (PList ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just p -> select (Just is) p
select (Just (i:is)) (PMap ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just (_, p) -> select (Just is) p

unsnocMay :: [a] -> Maybe ([a], a)
unsnocMay as = (,) <$> initMay as <*> lastMay as

makeNewSel ::
       (([Int], Int) -> [Int]) -> Maybe [Int] -> PersonProperty -> Maybe [Int]
makeNewSel func msel prop =
    case msel of
        Nothing -> Just [0]
        Just sel ->
            let newSel = func <$> unsnocMay sel
            in case select newSel prop of
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
