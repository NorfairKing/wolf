{-# LANGUAGE TypeFamilies #-}

module Wolf.Cub.PropertyEditor.Cursor
    ( ACursor(..)
    , PropertyCursor(..)
    , ValCursor
    , valCursorSelected
    , cursor
    , rebuild
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
    | AMKVC KeyValCursor

cursor :: PersonProperty -> ACursor
cursor = APropC . propertyCursor Nothing

instance Rebuild ACursor where
    rebuild (APropC pc) = rebuild pc
    rebuild (ALElC alec) = rebuild alec
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
            , listElCursorPrevElems = reverse $ take (i - 1) els
            , listElCursorNextElems = drop i els
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
            , keyValCursorPrevElems = reverse $ take (i - 1) els
            , keyValCursorNextElems = drop i els
            , keyValCursorKey = k
            , keyValCursorValue = pc
            }
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
    , keyValCursorKey :: Text
    , keyValCursorValue :: PropertyCursor
    }

instance Rebuild KeyValCursor where
    rebuild = rebuild . keyValCursorParent

instance Build KeyValCursor where
    type Building KeyValCursor = (Text, PersonProperty)
    build kvc = (keyValCursorKey kvc, build $ keyValCursorValue kvc)
-- class HasParentCursor a where
--     parent :: a -> Maybe ParentCursor
--
-- instance HasParentCursor ValCursor where
--     parent = valCursorParent
--
-- instance HasParentCursor ListCursor where
--     parent = listCursorParent
--
-- instance HasParentCursor MapCursor where
--     parent = mapCursorParent
