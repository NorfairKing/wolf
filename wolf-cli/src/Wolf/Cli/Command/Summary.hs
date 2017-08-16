{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Summary where

import Import

import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils
import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex
import Wolf.Data.Time

summary :: (MonadIO m, MonadReader Settings m) => Text -> m ()
summary person =
    runData $
    withInitCheck $ do
        index <- getIndexWithDefault
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
                    T.unpack $
                    formatMomentNicely now (personNoteTimestamp pn) <> ":"
                  , fromString $ T.unpack $ personNoteContents pn
                  ]
        , case mpe of
              Nothing -> "No person entry."
              Just pe ->
                  unlinesReport $
                  flip map (personEntryTuples pe) $ \(prop, val) ->
                      fromString $
                      unwords
                          [ T.unpack prop ++ ":"
                          , T.unpack $ personPropertyValueContents val
                          ]
        ]
