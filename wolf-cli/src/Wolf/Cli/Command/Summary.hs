{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Summary
    ( summary
    , printSummaryReportFor
    , summaryReportFor
    , summaryReport
    , SummaryReport(..)
    , summaryReportReport
    ) where

import Import

import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Time

summary :: (MonadIO m, MonadReader Settings m) => Alias -> m ()
summary person =
    runData $
    withInitCheck_ $ do
        index <- getIndexWithDefault
        case lookupInIndex person index of
            Nothing ->
                liftIO $
                die $ unwords ["No person found for", aliasString person]
            Just personUuid -> printSummaryReportFor personUuid

printSummaryReportFor ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m ()
printSummaryReportFor personUuid = do
    sr <- summaryReportFor personUuid
    liftIO $ putReport $ summaryReportReport sr

summaryReportFor ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m SummaryReport
summaryReportFor personUuid = do
    now <- liftIO getCurrentTime
    mpe <- getPersonEntry personUuid
    pns <- getPersonNotes personUuid
    pure $ summaryReport now mpe pns

summaryReport :: UTCTime -> Maybe PersonEntry -> [Note] -> SummaryReport
summaryReport now mpe pns =
    SummaryReport
    { summaryReportTimestamp = now
    , summaryReportPersonEntry = mpe
    , summaryReportNotes = sortOn noteTimestamp pns
    }

data SummaryReport = SummaryReport
    { summaryReportTimestamp :: UTCTime
    , summaryReportPersonEntry :: Maybe PersonEntry
    , summaryReportNotes :: [Note]
    } deriving (Show, Eq, Generic)

instance Validity SummaryReport where
    isValid SummaryReport {..} =
        and
            [ isValid summaryReportTimestamp
            , isValid summaryReportPersonEntry
            , isValid summaryReportNotes
            , sortOn noteTimestamp summaryReportNotes == summaryReportNotes
            ]

summaryReportReport :: SummaryReport -> Report
summaryReportReport SummaryReport {..} =
    mconcat
        [ mconcat $
          flip map summaryReportNotes $ \pn ->
              unlinesReport
                  [ colored [SetColor Foreground Dull Blue] $
                    formatMomentNicely summaryReportTimestamp (noteTimestamp pn) <>
                    ":"
                  , textReport $ noteContents pn
                  ]
        , case summaryReportPersonEntry of
              Nothing -> "No person entry."
              Just pe -> textReport $ entryContents pe
        , "\n"
        ]
