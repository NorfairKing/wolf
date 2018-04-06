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
            Just personUuid ->
                getIndexWithDefault >>= printSummaryReportFor personUuid

printSummaryReportFor ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> Index -> m ()
printSummaryReportFor personUuid ix = do
    sr <- summaryReportFor personUuid ix
    liftIO $ putReport $ summaryReportReport sr

summaryReportFor ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> Index
    -> m SummaryReport
summaryReportFor personUuid ix = do
    now <- liftIO getCurrentTime
    let as = reverseIndexLookup personUuid ix
    mpe <- getPersonEntry personUuid
    pns <- getPersonNotes personUuid
    pure $ summaryReport now as mpe pns

summaryReport ::
       UTCTime -> [Alias] -> Maybe PersonEntry -> [Note] -> SummaryReport
summaryReport now as mpe pns =
    SummaryReport
    { summaryReportTimestamp = now
    , summaryReportAliases = as
    , summaryReportPersonEntry = mpe
    , summaryReportNotes = sortOn noteTimestamp pns
    }

data SummaryReport = SummaryReport
    { summaryReportTimestamp :: UTCTime
    , summaryReportAliases :: [Alias]
    , summaryReportPersonEntry :: Maybe PersonEntry
    , summaryReportNotes :: [Note]
    } deriving (Show, Eq, Generic)

instance Validity SummaryReport where
    isValid SummaryReport {..} =
        and
            [ isValid summaryReportTimestamp
            , isValid summaryReportAliases
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
        , case summaryReportAliases of
              [] -> "No aliases"
              [a] -> "Alias: " <> textReport (aliasText a)
              as ->
                  unlinesReport $
                  "Aliases:" : map (textReport . (" - " <>) . aliasText) as
        , "\n"
        ]
