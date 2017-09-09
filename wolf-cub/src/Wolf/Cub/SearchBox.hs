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

data SearchBox n = SearchBox
    { searchBoxName :: n
    , searchBoxCurrentContent :: Text
    , searchBoxSelectable :: [Text]
    } deriving (Show, Eq, Generic)

searchBox :: n -> [Text] -> SearchBox n
searchBox name ts =
    SearchBox
    { searchBoxName = name
    , searchBoxCurrentContent = ""
    , searchBoxSelectable = ts
    }

searchBoxCurrentlySelected :: SearchBox n -> [Text]
searchBoxCurrentlySelected SearchBox {..} =
    filter (T.isInfixOf searchBoxCurrentContent) searchBoxSelectable

renderSearchBox :: SearchBox n -> Widget n
renderSearchBox SearchBox {..} = txt searchBoxCurrentContent

handleSearchBox :: SearchBox n -> Event -> SearchBox n
handleSearchBox sb@SearchBox {..} e =
    case e of
        (EvKey (KChar c) []) ->
            sb
            {searchBoxCurrentContent = searchBoxCurrentContent <> T.singleton c}
        _ -> sb
