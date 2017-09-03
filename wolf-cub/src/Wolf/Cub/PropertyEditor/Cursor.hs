module Wolf.Cub.PropertyEditor.Cursor
    ( PropertyCursor
    , cursor
    , rebuild
    ) where

import Import

import Wolf.Data

data PropertyCursor
    = ValC ValCursor
    | ListC ListCursor
    | MapC MapCursor

cursor :: PersonProperty -> PropertyCursor
cursor prop =
    case prop of
        PVal v ->
            ValC $ ValCursor {valCursorParent = Nothing, valCursorSelected = v}
        PList ls -> ListC $ listCursor Nothing ls
        PMap ls -> MapC $ mapCursor Nothing ls

data ParentCursor
    = ListElPC ListElCursor
    | KeyValPC KeyValCursor

data ValCursor = ValCursor
    { valCursorParent :: Maybe ParentCursor
    , valCursorSelected :: PersonPropertyValue
    }

data ListCursor = ListCursor
    { listCursorParent :: Maybe ParentCursor
    , listCursorElems :: [ListElCursor]
    , listCursorSelected :: [PersonProperty]
    }

listCursor :: Maybe ParentCursor -> [PersonProperty] -> ListCursor
listCursor par ls = lc
  where
    lc =
        ListCursor
        {listCursorParent = par, listCursorElems = els, listCursorSelected = ls}
    indexLs = zip [0 ..] ls
    els = map (uncurry el) indexLs
    el i l =
        ListElCursor
        { listElCursorParent = lc
        , listElCursorPrevElems = reverse $ take (i - 1) els
        , listElCursorNextElems = drop i els
        , listElCursorIx = i
        , listElCursorSelected = l
        }

data MapCursor = MapCursor
    { mapCursorParent :: Maybe ParentCursor
    , mapCursorElems :: [KeyValCursor]
    , mapCursorSelected :: [(Text, PersonProperty)]
    }

mapCursor :: Maybe ParentCursor -> [(Text, PersonProperty)] -> MapCursor
mapCursor par ls = mc
  where
    mc =
        MapCursor
        {mapCursorParent = par, mapCursorElems = els, mapCursorSelected = ls}
    indexLs = zip [0 ..] ls
    els = map (uncurry el) indexLs
    el i (k, v) =
        KeyValCursor
        { keyValCursorParent = mc
        , keyValCursorPrevElems = reverse $ take (i - 1) els
        , keyValCursorNextElems = drop i els
        , keyValCursorKey = k
        , keyValCursorSelected = v
        }

data ListElCursor = ListElCursor
    { listElCursorParent :: ListCursor
    , listElCursorPrevElems :: [ListElCursor] -- ^ In reverse order, so that the first element is the nearest.
    , listElCursorNextElems :: [ListElCursor]
    , listElCursorIx :: Int
    , listElCursorSelected :: PersonProperty
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

data KeyValCursor = KeyValCursor
    { keyValCursorParent :: MapCursor
    , keyValCursorPrevElems :: [KeyValCursor]
    , keyValCursorNextElems :: [KeyValCursor]
    , keyValCursorKey :: Text
    , keyValCursorSelected :: PersonProperty
    }

class HasParentCursor a where
    parent :: a -> Maybe ParentCursor

instance HasParentCursor ValCursor where
    parent = valCursorParent

instance HasParentCursor ListCursor where
    parent = listCursorParent

instance HasParentCursor MapCursor where
    parent = mapCursorParent

class Rebuild a where
    rebuild :: a -> PersonProperty

instance Rebuild PropertyCursor where
    rebuild (ValC vc) = rebuild vc
    rebuild (ListC lc) = rebuild lc
    rebuild (MapC mc) = rebuild mc

instance Rebuild ParentCursor where
    rebuild (ListElPC lc) = rebuild lc
    rebuild (KeyValPC kvpc) = rebuild kvpc

instance Rebuild ValCursor where
    rebuild = PVal . valCursorSelected

instance Rebuild ListCursor where
    rebuild = PList . listCursorSelected

instance Rebuild MapCursor where
    rebuild = PMap . mapCursorSelected

instance Rebuild ListElCursor where
    rebuild = rebuild . listElCursorParent

instance Rebuild KeyValCursor where
    rebuild = rebuild . keyValCursorParent
