{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Time
    ( formatMomentNicely
    , timeAgoStr
    ) where

import Import

import qualified Data.Text as T

import Data.Time

-- | Convenience function for showing time in a nicely readable way.
--
-- The first argument is the current time, and the second is the time to format.
-- The result will look something like "Sunday 2017-08-06 11:37 (10 days ago)"
formatMomentNicely :: UTCTime -> UTCTime -> Text
formatMomentNicely now t =
    T.unwords
        [ T.pack $ formatTime defaultTimeLocale "%A %F %R" t
        , T.concat ["(", timeAgoStr now t, ")"]
        ]

-- | Convenience function to show how long ago a certain point in time was.
--
-- The first argument is the current time, and the second is the time to format.
-- The result will look something like "10 days ago"
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
