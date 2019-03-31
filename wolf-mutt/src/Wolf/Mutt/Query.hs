{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Mutt.Query
  ( wolfMuttQuery
  , SearchQuery
  , SearchResult(..)
  , searchResultsFor
  , formatSearchResult
  , formatSearchResults
  ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Control.Monad.Reader

import Text.Email.Validate as EmailAddress (toByteString)

import Wolf.Data
import Wolf.Data.Baked (EmailAddressWithPurpose(..), fromEntry)

import Wolf.Mutt.OptParse
import Wolf.Mutt.Query.Types

wolfMuttQuery :: (MonadIO m, MonadReader Settings m) => SearchQuery -> m ()
wolfMuttQuery q = do
  dataSets <- asks setDataSets
  results <-
    flip runReaderT dataSets $ do
      ix <- getIndexWithDefault
      fmap concat $
        forM (indexTuples ix) $ \(a, uuid) ->
          if matchesAlias q a
            then do
              mpe <- getPersonEntry uuid
              pure $ searchResultsFor a mpe
            else pure []
  liftIO $ TIO.putStr $ formatSearchResults results

searchResultsFor :: Alias -> Maybe PersonEntry -> [SearchResult]
searchResultsFor _ Nothing = []
searchResultsFor a (Just pe) = do
  EmailAddressWithPurpose mpur ea <- fromEntry pe :: [EmailAddressWithPurpose]
  pure
    SearchResult
      { searchResultEmailAddress = TE.decodeUtf8 $ EmailAddress.toByteString ea
      , searchResultLongName = aliasText a
      , searchResultOtherInfo = fromMaybe "" mpur
      }

matchesAlias :: SearchQuery -> Alias -> Bool
matchesAlias q = matchesText q . aliasText

matchesText :: SearchQuery -> Text -> Bool
matchesText q t = T.toLower q `T.isInfixOf` T.toLower t

-- According to the docs:
--
-- > It is the program executed when calling the function <query> and <complete-query>, that has to take as argument a string, representing the search string, and has to print a textual output consisting of lines of the format:
-- > `<email address> <tab> <long name> <tab> <other info> <newline>`
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

formatSearchResults :: [SearchResult] -> Text
formatSearchResults results = T.concat $ "\n" : map formatSearchResult results
    -- This newline was not in the spec but still necessary for some reason.
