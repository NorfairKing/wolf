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
import Data.Time

import Graphics.Vty as Vty
import Graphics.Vty as V

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit

import Wolf.Data

import Wolf.Cub.PropertyEditor.Cursor

data PropertyEditor n = PropertyEditor
    { propertyEditorName :: n
    , propertyEditorCursor :: Maybe ACursor
    , propertyEditorCurrentEditor :: Maybe (Editor Text n)
    } deriving (Generic)

propertyEditor :: n -> Maybe PersonProperty -> PropertyEditor n
propertyEditor name mprop =
    PropertyEditor
    { propertyEditorName = name
    , propertyEditorCursor = cursor <$> mprop
    , propertyEditorCurrentEditor = Nothing
    }

renderPropertyEditor :: (Show n, Ord n) => PropertyEditor n -> Widget n
renderPropertyEditor PropertyEditor {..} =
    addDebugInfo $
    withAttr propertyEditorAttr $
    vBox
        [ case rebuild <$> propertyEditorCursor of
              Nothing -> txt "No properties, press 's' to start a new property."
              Just pp ->
                  padRight Max $
                  padBottom Max $ go (makeSelection <$> propertyEditorCursor) pp
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
        flip map (zip [0 ..] vs) $ \(ix, v) ->
            let thisSel = drillSel msel ix
                withListElemAttr = withSelectedAttr thisSel
                dashSel = thisSel
                withDashAttr = withSelectedAttr dashSel
                valueSel = drillSel thisSel 0
                withValueAttr = withSelectedAttr valueSel
                dash = withDashAttr $ txt "- "
                valueSide = withValueAttr $ go valueSel v
            in withListElemAttr $ dash <+> valueSide
    go msel (PMap tups) =
        withSelectedAttr msel $
        vBox $
        flip map (zip [0 ..] tups) $ \(ix, (k, v)) ->
            let thisSel = drillSel msel ix
                withKeyValueAttr = withSelectedAttr thisSel
                keySel = drillSel thisSel 0
                withKeyAttr = withSelectedAttr keySel
                valueSel = drillSel thisSel 1
                withValueAttr = withSelectedAttr valueSel
                keySide = withKeyAttr $ txt k
                mid = txt ": "
                leftSide = keySide <+> mid
                valueSide = withValueAttr $ go valueSel v
            in withKeyValueAttr $
               case v of
                   (PVal _) -> leftSide <+> valueSide
                   _ -> leftSide <=> padLeft (Pad 2) valueSide
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
    addDebugInfo =
        (<=> withAttr
                 propertyEditorAttrSelected
                 (case propertyEditorCursor of
                      Nothing -> emptyWidget
                      Just cur ->
                          strWrap $
                          case cur of
                              APropC pc -> show $ build pc
                              ALElC lec -> show $ build lec
                              AKC kc -> show $ build kc
                              AMKVC kvc -> show $ build kvc -- TODO remove this.
                  ))
    -- ((withAttr
    --       propertyEditorAttrSelected
    --       (str (show $ propertyEditorSelection <$> cur)) <=>
    --   str " ") <=>) $ -- TODO remove this.
    -- (<=> withAttr
    --          propertyEditorAttrSelected
    --          (strWrap
    --               (show $
    --                select propertyEditorSelection <$>
    --                (rebuild <$> propertyEditorCursor)))) $ -- TODO remove this.

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
    case rebuild <$> propertyEditorCursor of
        Nothing ->
            case e of
                (EvKey (V.KChar 's') []) ->
                    pure pe {propertyEditorCursor = Just $ cursor emptyProperty}
                _ -> pure pe
        Just _ ->
            case propertyEditorCurrentEditor of
                Nothing ->
                    case e of
                        (EvKey KDown []) -> moveDown pe
                        (EvKey KUp []) -> moveUp pe
                        (EvKey KLeft []) -> moveLeft pe
                        (EvKey KRight []) -> moveRight pe
                        (EvKey KEnter []) -> tryToStartSubEditor pe
                        (EvKey (KChar 'e') []) -> tryToStartSubEditor pe
                        _ -> pure pe
                Just ed ->
                    case e of
                        (EvKey KEnter []) -> tryToQuitAndSaveEditor pe ed
                        _ -> do
                            ne <- handleEditorEvent e ed
                            pure $ pe {propertyEditorCurrentEditor = Just ne}

tryToStartSubEditor ::
       Monoid n => PropertyEditor n -> EventM n (PropertyEditor n)
tryToStartSubEditor pe@PropertyEditor {..} =
    case propertyEditorCursor of
        Nothing -> pure pe
        Just cur ->
            let mContents =
                    case cur of
                        APropC (ValC vc) ->
                            Just $
                            personPropertyValueContents $ valCursorSelected vc
                        AKC kc -> Just $ keyCursorSelected kc
                        _ -> Nothing
            in case mContents of
                   Nothing -> pure pe
                   Just cts ->
                       pure $
                       pe
                       { propertyEditorCurrentEditor =
                             Just $
                             editorText
                                 (propertyEditorName <>
                                  propertyEditorName -- Weird hack to get the name to be unique.
                                  )
                                 (Just 1)
                                 cts
                       }

tryToQuitAndSaveEditor ::
       PropertyEditor n -> Editor Text t -> EventM n (PropertyEditor n)
tryToQuitAndSaveEditor pe@PropertyEditor {..} ed =
    case propertyEditorCursor of
        Nothing -> pure pe
        Just cur -> do
            let contents = T.concat $ getEditContents ed
            case cur of
                APropC (ValC vc) -> do
                    now <- liftIO getCurrentTime
                    let newValue =
                            PersonPropertyValue
                            { personPropertyValueLastUpdatedTimestamp = now
                            , personPropertyValueContents = contents
                            }
                    pure $
                        pe
                        { propertyEditorCurrentEditor = Nothing
                        , propertyEditorCursor =
                              Just $
                              APropC $
                              ValC $ valCursorModifyValue (const newValue) vc
                        }
                AKC kc ->
                    pure $
                    pe
                    { propertyEditorCurrentEditor = Nothing
                    , propertyEditorCursor =
                          Just $ AKC $ keyCursorModifyKey (const contents) kc
                    }
                _ -> pure pe

moveUp :: PropertyEditor n -> EventM n (PropertyEditor n)
moveUp = moveCursor cursorUp

moveDown :: PropertyEditor n -> EventM n (PropertyEditor n)
moveDown = moveCursor cursorDown

moveLeft :: PropertyEditor n -> EventM n (PropertyEditor n)
moveLeft = moveCursor cursorLeft

moveRight :: PropertyEditor n -> EventM n (PropertyEditor n)
moveRight = moveCursor cursorRight

moveCursor ::
       (ACursor -> Maybe ACursor)
    -> PropertyEditor n
    -> EventM n (PropertyEditor n)
moveCursor move pe =
    pure
        pe
        { propertyEditorCursor =
              case propertyEditorCursor pe of
                  Nothing -> Nothing
                  Just cur -> Just $ fromMaybe cur $ move cur
        }
