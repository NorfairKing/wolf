{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub.SearchBox
    ( SearchBox
    , searchBox
    , searchBoxCurrentlySelected
    , renderSearchBox
    , handleSearchBox
    ) where

import Import

import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty as V

import qualified Data.Text as T

data SearchBox n a = SearchBox
    { searchBoxName :: n
    , searchBoxCurrentContent :: Text
    , searchBoxSelectable :: [(Text, a)]
    } deriving (Show, Eq, Generic)

searchBox :: n -> [(Text, a)] -> SearchBox n a
searchBox name ts =
    SearchBox
    { searchBoxName = name
    , searchBoxCurrentContent = ""
    , searchBoxSelectable = ts
    }

searchBoxCurrentlySelected :: SearchBox n a -> [(Text, a)]
searchBoxCurrentlySelected SearchBox {..} =
    filter (T.isInfixOf searchBoxCurrentContent . fst) searchBoxSelectable

renderSearchBox :: SearchBox n a -> Widget n
renderSearchBox SearchBox {..} = txt "Search: " <+> txt searchBoxCurrentContent

handleSearchBox :: SearchBox n a -> Event -> SearchBox n a
handleSearchBox sb e =
    case e of
        (EvKey (KChar c) []) ->
            sb
            {searchBoxCurrentContent = searchBoxCurrentContent sb <> T.pack [c]}
        (EvKey KBS []) ->
            sb
            {searchBoxCurrentContent = T.dropEnd 1 $ searchBoxCurrentContent sb}
        _ -> sb
