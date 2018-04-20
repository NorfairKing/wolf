{-# LANGUAGE DeriveGeneric #-}

module Wolf.Mutt.Query.Types
    ( SearchQuery
    , SearchResult(..)
    ) where

import Import

type SearchQuery = String

data SearchResult = SearchResult
    { searchResultEmailAddress :: Text
    , searchResultLongName :: Text
    , searchResultOtherInfo :: Text
    } deriving (Show, Eq, Generic)

instance Validity SearchResult
