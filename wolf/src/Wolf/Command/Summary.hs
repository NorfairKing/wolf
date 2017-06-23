{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Summary where

import Import

import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Index
import Wolf.Init
import Wolf.NoteIndex
import Wolf.OptParse.Types
import Wolf.Report
import Wolf.Time
import Wolf.Types

summary :: (MonadIO m, MonadReader Settings m) => String -> m ()
summary person =
    withInitCheck $ do
        index <- getIndex
        case lookupInIndex person index of
            Nothing ->
                liftIO $ die $ unwords ["No person found for", show person]
            Just personUuid -> do
                now <- liftIO getCurrentTime
                mpe <- getPersonEntry personUuid
                pns <- getPersonNotes personUuid
                liftIO $ putStr $ renderReport $ summaryReport now mpe pns

summaryReport :: UTCTime -> Maybe PersonEntry -> [PersonNote] -> Report
summaryReport now mpe pns =
    mconcat
        [ mconcat $
          flip map pns $ \pn ->
              unlinesReport
                  [ colored [SetColor Foreground Dull Blue] $
                    formatMomentNicely now (personNoteTimestamp pn) ++ ":"
                  , fromString $ T.unpack $ personNoteContents pn
                  ]
        , case mpe of
              Nothing -> "No person entry."
              Just pe ->
                  unlinesReport $
                  flip map (personEntryProperties pe) $ \(prop, val) ->
                      fromString $
                      unwords [prop ++ ":", personPropertyValueContents val]
        ]
