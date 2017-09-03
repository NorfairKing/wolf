{-# LANGUAGE TypeFamilies #-}

module Wolf.Cub.PropertyEditor.Cursor
    ( ACursor(..)
    , PropertyCursor(..)
    , ValCursor
    , valCursorSelected
    , valCursorModifyValue
    , cursor
    , build
    , rebuild
    , listCursorElems
    , mapCursorElems
    , listElCursorValue
    , keyValCursorKey
    , keyValCursorValue
    , keyCursorParent
    , keyCursorSelected
    , makeSelection
    , cursorUp
    , cursorDown
    , cursorLeft
    , cursorRight
    ) where

import Import

import Wolf.Data

class Rebuild a where
    rebuild :: a -> PersonProperty

class Build a where
    type Building a :: *
    build :: a -> Building a

data ACursor
    = APropC PropertyCursor
    | ALElC ListElCursor
    | AKC KeyCursor
    | AMKVC KeyValCursor

cursor :: PersonProperty -> ACursor
cursor = APropC . propertyCursor Nothing

instance Rebuild ACursor where
    rebuild (APropC pc) = rebuild pc
    rebuild (ALElC alec) = rebuild alec
    rebuild (AKC kc) = rebuild kc
    rebuild (AMKVC akvc) = rebuild akvc

data PropertyCursor
    = ValC ValCursor
    | ListC ListCursor
    | MapC MapCursor

instance Rebuild PropertyCursor where
    rebuild (ValC vc) = rebuild vc
    rebuild (ListC lc) = rebuild lc
    rebuild (MapC mc) = rebuild mc

instance Build PropertyCursor where
    type Building PropertyCursor = PersonProperty
    build (ValC vc) = PVal $ build vc
    build (ListC lc) = PList $ build lc
    build (MapC mc) = PMap $ build mc

propertyCursor :: Maybe ParentCursor -> PersonProperty -> PropertyCursor
propertyCursor par prop =
    case prop of
        PVal v -> ValC $ valCursor par v
        PList ls -> ListC $ listCursor par ls
        PMap ls -> MapC $ mapCursor par ls

data ParentCursor
    = ListElPC ListElCursor
    | KeyValPC KeyValCursor

instance Rebuild ParentCursor where
    rebuild (ListElPC lc) = rebuild lc
    rebuild (KeyValPC kvpc) = rebuild kvpc

data ValCursor = ValCursor
    { valCursorParent :: Maybe ParentCursor
    , valCursorSelected :: PersonPropertyValue
    }

valCursor :: Maybe ParentCursor -> PersonPropertyValue -> ValCursor
valCursor par val = ValCursor {valCursorParent = par, valCursorSelected = val}

valCursorModifyValue ::
       (PersonPropertyValue -> PersonPropertyValue) -> ValCursor -> ValCursor
valCursorModifyValue = undefined

instance Rebuild ValCursor where
    rebuild vc =
        case valCursorParent vc of
            Nothing -> PVal $ build vc
            Just par -> rebuild par

instance Build ValCursor where
    type Building ValCursor = PersonPropertyValue
    build = valCursorSelected

data ListCursor = ListCursor
    { listCursorParent :: Maybe ParentCursor
    , listCursorElems :: [ListElCursor]
    }

listCursor :: Maybe ParentCursor -> [PersonProperty] -> ListCursor
listCursor par ls = lc
  where
    lc = ListCursor {listCursorParent = par, listCursorElems = els}
    indexLs = zip [0 ..] ls
    els = map (uncurry el) indexLs
    el i v = vec
      where
        vec =
            ListElCursor
            { listElCursorParent = lc
            , listElCursorPrevElems =
                  reverse $ filter ((< i) . listElCursorIx) els
            , listElCursorNextElems = filter ((> i) . listElCursorIx) els
            , listElCursorIx = i
            , listElCursorValue = pc
            }
        pc = propertyCursor (Just $ ListElPC vec) v

instance Rebuild ListCursor where
    rebuild lc =
        case listCursorParent lc of
            Nothing -> PList $ build lc
            Just par -> rebuild par

instance Build ListCursor where
    type Building ListCursor = [PersonProperty]
    build = map build . listCursorElems

data MapCursor = MapCursor
    { mapCursorParent :: Maybe ParentCursor
    , mapCursorElems :: [KeyValCursor]
    }

mapCursor :: Maybe ParentCursor -> [(Text, PersonProperty)] -> MapCursor
mapCursor par ls = mc
  where
    mc = MapCursor {mapCursorParent = par, mapCursorElems = els}
    indexLs = zip [0 ..] ls
    els = map (uncurry el) indexLs
    el i (k, v) = kvc
      where
        kvc =
            KeyValCursor
            { keyValCursorParent = mc
            , keyValCursorPrevElems =
                  reverse $ filter ((< i) . keyValCursorIx) els
            , keyValCursorNextElems = filter ((> i) . keyValCursorIx) els
            , keyValCursorIx = i
            , keyValCursorKey = kc
            , keyValCursorValue = pc
            }
        kc = keyCursor kvc k
        pc = propertyCursor (Just $ KeyValPC kvc) v

instance Rebuild MapCursor where
    rebuild mc =
        case mapCursorParent mc of
            Nothing -> PMap $ build mc
            Just p -> rebuild p

instance Build MapCursor where
    type Building MapCursor = [(Text, PersonProperty)]
    build = map build . mapCursorElems

data ListElCursor = ListElCursor
    { listElCursorParent :: ListCursor
    , listElCursorPrevElems :: [ListElCursor] -- ^ In reverse order, so that the first element is the nearest.
    , listElCursorNextElems :: [ListElCursor]
    , listElCursorIx :: Int
    , listElCursorValue :: PropertyCursor
    }

listElCursorSelectPrev :: ListElCursor -> Maybe ListElCursor
listElCursorSelectPrev lec =
    case listElCursorPrevElems lec of
        [] -> Nothing
        (lec':_) -> Just lec'

listElCursorSelectNext :: ListElCursor -> Maybe ListElCursor
listElCursorSelectNext lec =
    case listElCursorNextElems lec of
        [] -> Nothing
        (lec':_) -> Just lec'

instance Rebuild ListElCursor where
    rebuild = rebuild . listElCursorParent

instance Build ListElCursor where
    type Building ListElCursor = PersonProperty
    build = build . listElCursorValue

data KeyValCursor = KeyValCursor
    { keyValCursorParent :: MapCursor
    , keyValCursorPrevElems :: [KeyValCursor]
    , keyValCursorNextElems :: [KeyValCursor]
    , keyValCursorIx :: Int
    , keyValCursorKey :: KeyCursor
    , keyValCursorValue :: PropertyCursor
    }

instance Rebuild KeyValCursor where
    rebuild = rebuild . keyValCursorParent

instance Build KeyValCursor where
    type Building KeyValCursor = (Text, PersonProperty)
    build kvc = (build $ keyValCursorKey kvc, build $ keyValCursorValue kvc)

data KeyCursor = KeyCursor
    { keyCursorParent :: KeyValCursor
    , keyCursorSelected :: Text
    }

keyCursor :: KeyValCursor -> Text -> KeyCursor
keyCursor kvc t = KeyCursor {keyCursorParent = kvc, keyCursorSelected = t}

instance Rebuild KeyCursor where
    rebuild = rebuild . keyCursorParent

instance Build KeyCursor where
    type Building KeyCursor = Text
    build = keyCursorSelected

makeSelection :: ACursor -> [Int]
makeSelection ac =
    reverse $
    case ac of
        APropC pc -> mspc pc
        ALElC lec -> mslec lec
        AKC kc -> mskc kc
        AMKVC kvc -> mskvc kvc
  where
    mspc (ValC vc) = fromMaybe [] $ msparc <$> valCursorParent vc
    mspc (ListC lc) = mslc lc
    mspc (MapC mc) = msmc mc
    msparc (ListElPC lec) = 0 : mslec lec
    msparc (KeyValPC kvc) = 1 : mskvc kvc
    mslc lc = fromMaybe [] $ msparc <$> listCursorParent lc
    msmc mc = fromMaybe [] $ msparc <$> mapCursorParent mc
    mslec lec = listElCursorIx lec : mslc (listElCursorParent lec)
    mskc kc = 0 : mskvc (keyCursorParent kc)
    mskvc kvc = keyValCursorIx kvc : msmc (keyValCursorParent kvc)

cursorUp :: ACursor -> Maybe ACursor
cursorUp cur =
    case cur of
        APropC _ -> Nothing -- Cannot go up in a property
        ALElC lec ->
            case listElCursorPrevElems lec of
                [] -> Nothing -- Cannot go down past the end of a list
                (nlec:_) -> pure $ ALElC nlec
        AKC _ -> Nothing -- Cannot go up in a key
        AMKVC kvc ->
            case keyValCursorPrevElems kvc of
                [] -> Nothing -- Cannot go up past the end of a map
                (nkvc:_) -> pure $ AMKVC nkvc

cursorDown :: ACursor -> Maybe ACursor
cursorDown cur =
    case cur of
        APropC _ -> Nothing -- Cannot go down in a property
        ALElC lec ->
            case listElCursorNextElems lec of
                [] -> Nothing -- Cannot go down past the end of a list
                (nlec:_) -> pure $ ALElC nlec
        AKC _ -> Nothing -- Cannot go down in a key
        AMKVC kvc ->
            case keyValCursorNextElems kvc of
                [] -> Nothing -- Cannot go down past the end of a map
                (nkvc:_) -> pure $ AMKVC nkvc

cursorLeft :: ACursor -> Maybe ACursor
cursorLeft cur =
    case cur of
        APropC pc -> do
            let parentCursor pc =
                    case pc of
                        Nothing -> Nothing
                        Just (ListElPC lec) -> Just $ ALElC lec
                        Just (KeyValPC kvc) -> Just $ AMKVC kvc
            case pc of
                ValC vc -> parentCursor $ valCursorParent vc
                ListC lc -> parentCursor $ listCursorParent lc
                MapC mc -> parentCursor $ mapCursorParent mc
        ALElC lec -> Just $ APropC $ ListC $ listElCursorParent lec
        AKC kc -> Just $ AMKVC $ keyCursorParent kc
        AMKVC kvc -> Just $ APropC $ MapC $ keyValCursorParent kvc

cursorRight :: ACursor -> Maybe ACursor
cursorRight cur =
    case cur of
        APropC pc ->
            case pc of
                ValC vc -> Nothing
                ListC lc ->
                    case listCursorElems lc of
                        [] -> Nothing
                        (lec:_) -> Just $ ALElC lec -- The first list element.
                MapC mc ->
                    case mapCursorElems mc of
                        [] -> Nothing
                        (kvc:_) -> Just $ AMKVC kvc -- The first map element.
        ALElC lec -> Just $ APropC $ listElCursorValue lec
        AKC kc -> Just $ APropC $ keyValCursorValue $ keyCursorParent kc
        AMKVC kvc -> Just $ AKC $ keyValCursorKey kvc
