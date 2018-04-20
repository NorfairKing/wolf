{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Mutt.Query
    ( wolfMuttQuery
    , SearchQuery
    , SearchResult(..)
    , formatSearchResult
    ) where

import Import

import qualified Data.Text as T

import Wolf.Mutt.Query.OptParse
import Wolf.Mutt.Query.Types

wolfMuttQuery :: IO ()
wolfMuttQuery = do
    i <- getInstructions
    print i

formatSearchResult :: SearchResult -> Text
formatSearchResult SearchResult {..} =
    T.concat
        [ searchResultEmailAddress
        , "\t"
        , searchResultLongName
        , "\t"
        , searchResultOtherInfo
        , "\n"
        ]
