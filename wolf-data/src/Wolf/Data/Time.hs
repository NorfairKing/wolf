{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Time
    ( formatMomentNicely
    , timeAgoStr
    ) where

import Import

import qualified Data.Text as T

import Data.Time

formatMomentNicely :: UTCTime -> UTCTime -> Text
formatMomentNicely now t =
    T.unwords
        [ T.pack $ formatTime defaultTimeLocale "%A %F %R" t
        , T.concat ["(", timeAgoStr now t, ")"]
        ]

timeAgoStr :: UTCTime -> UTCTime -> Text
timeAgoStr now t
    | daysAgo > 0 = T.pack $ unwords [show daysAgo, "days ago"]
    | hoursAgo > 0 = T.pack $ unwords [show hoursAgo, "hours ago"]
    | otherwise = T.pack $ unwords [show minutesAgo, "minutes ago"]
  where
    minutesAgo = round $ dt / 60 :: Int
    hoursAgo = round $ dt / (60 * 60) :: Int
    daysAgo = round $ dt / (24 * 60 * 60) :: Int
    dt = diffUTCTime now t
