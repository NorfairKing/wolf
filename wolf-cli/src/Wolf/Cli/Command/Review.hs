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

review :: (MonadIO m, MonadReader Settings m) => m ()
review =
    runData $
    withInitCheck_ $ do
        rr <- makeReviewReport
        liftIO $ putReport $ reviewReportReport rr

makeReviewReport :: (MonadIO m, MonadReader DataSettings m) => m ReviewReport
makeReviewReport = do
    ns <- getNotes
    ix <- getIndexWithDefault
    now <- liftIO getCurrentTime
    pure $ reviewReport now ix ns

reviewReport :: UTCTime -> Index -> [Note] -> ReviewReport
reviewReport now ix ns =
    let noteTups =
            M.fromList $
            flip map ns $ \n@Note {..} ->
                let as =
                        flip map (S.toList noteRelevantPeople) $ \uuid ->
                            fromMaybe
                                (personUuidText uuid)
                                (aliasText <$>
                                 reverseIndexLookupSingleAlias uuid ix)
                in (n, as)
    in ReviewReport {reviewReportTimestamp = now, reviewReportNotes = noteTups}

data ReviewReport = ReviewReport
    { reviewReportTimestamp :: UTCTime
    , reviewReportNotes :: Map Note [Text]
    } deriving (Show, Eq, Generic)

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
               ]
