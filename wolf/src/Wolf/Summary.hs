{-# LANGUAGE OverloadedStrings #-}

module Wolf.Summary where

import Import

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Index
import Wolf.NoteIndex
import Wolf.Report
import Wolf.Types

summary :: String -> IO ()
summary person = do
    index <- getIndex
    case lookupInIndex person index of
        Nothing -> die $ unwords ["No person found for", show person]
        Just personUuid -> do
            now <- getCurrentTime
            mpe <- getPersonEntry personUuid
            pns <- getPersonNotes personUuid
            putStr $ renderReport $ summaryReport now mpe pns

summaryReport :: UTCTime -> Maybe PersonEntry -> [PersonNote] -> Report
summaryReport now mpe pns =
    mconcat
        [ mconcat $
          flip map pns $ \pn ->
              unlinesReport
                  [ colored [SetColor Foreground Dull Blue] $
                    formatTimeStr (personNoteTimestamp pn) ++ ":"
                  , fromString $ T.unpack $ personNoteContents pn
                  ]
        , case mpe of
              Nothing -> "No person entry."
              Just pe ->
                  unlinesReport $
                  flip map (M.toList $ personEntryProperties pe) $ \(prop, val) ->
                      fromString $ unwords [prop ++ ":", val]
        ]
  where
    formatTimeStr t =
        unwords [formatTime defaultTimeLocale "%A %F %R" t, timeAgoStr t]
    timeAgoStr t
        | daysAgo > 0 = unwords ["(" ++ show daysAgo, "days ago)"]
        | hoursAgo > 0 = unwords ["(" ++ show hoursAgo, "hours ago)"]
        | otherwise = unwords ["(" ++ show minutesAgo, "minutes ago)"]
      where
        minutesAgo = round $ dt / 60 :: Int
        hoursAgo = round $ dt / (60 * 60) :: Int
        daysAgo = round $ dt / (24 * 60 * 60) :: Int
        dt = diffUTCTime now t
