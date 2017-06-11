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
import Wolf.Time
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
                    formatMomentNicely now (personNoteTimestamp pn) ++ ":"
                  , fromString $ T.unpack $ personNoteContents pn
                  ]
        , case mpe of
              Nothing -> "No person entry."
              Just pe ->
                  unlinesReport $
                  flip map (M.toList $ personEntryProperties pe) $ \(prop, val) ->
                      fromString $ unwords [prop ++ ":", val]
        ]
