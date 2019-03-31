{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Review where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Report
import Wolf.Cli.Utils
import Wolf.Data
import Wolf.Data.Time

review :: (MonadIO m, MonadReader Settings m) => PeriodDescription -> m ()
review pd =
  runData $
  withInitCheck_ $ do
    rr <- reviewReportFor pd
    liftIO $ putReport $ reviewReportReport rr

reviewReportFor ::
     (MonadIO m, MonadReader DataSettings m)
  => PeriodDescription
  -> m ReviewReport
reviewReportFor pd = do
  ns <- getNotes
  ix <- getIndexWithDefault
  now <- liftIO getCurrentTime
  pure $ reviewReport pd now ix ns

reviewReport :: PeriodDescription -> UTCTime -> Index -> [Note] -> ReviewReport
reviewReport pd now ix ns =
  let noteTups =
        M.fromList $
        flip map (filter (inPeriod now pd . noteTimestamp) ns) $ \n@Note {..} ->
          let as =
                flip map (S.toList noteRelevantPeople) $ \uuid ->
                  maybe
                    (uuidText uuid)
                    aliasText
                    (reverseIndexLookupSingleAlias uuid ix)
           in (n, as)
   in ReviewReport {reviewReportTimestamp = now, reviewReportNotes = noteTups}

inPeriod :: UTCTime -> PeriodDescription -> UTCTime -> Bool
inPeriod now pd time = now `diffUTCTime` time <= days
  where
    days =
      (* oneDay) $
      case pd of
        LastDay -> 1
        LastWeek -> 7
        LastMonth -> 30
    oneDay = 60 * 60 * 24 -- seconds

data ReviewReport =
  ReviewReport
    { reviewReportTimestamp :: UTCTime
    , reviewReportNotes :: Map Note [Text]
    }
  deriving (Show, Eq, Generic)

instance Validity ReviewReport

reviewReportReport :: ReviewReport -> Report
reviewReportReport ReviewReport {..} =
  let entries = sortOn (noteTimestamp . fst) $ M.toList reviewReportNotes
   in mconcat $
      flip map entries $ \(Note {..}, names) ->
        unlinesReport
          [ colored [SetColor Foreground Dull Blue] $
            T.unwords
              [ T.intercalate ", " names <> ","
              , formatMomentNicely reviewReportTimestamp noteTimestamp
              ]
          , textReport noteContents
          , mempty
          ]
