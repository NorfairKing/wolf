{-# LANGUAGE OverloadedStrings #-}

module Wolf.Time
    ( formatMomentNicely
    , timeAgoStr
    ) where

import Import

import Data.Time

formatMomentNicely :: UTCTime -> UTCTime -> String
formatMomentNicely now t =
    unwords
        [ formatTime defaultTimeLocale "%A %F %R" t
        , "(" ++ timeAgoStr now t ++ ")"
        ]

timeAgoStr :: UTCTime -> UTCTime -> String
timeAgoStr now t
    | daysAgo > 0 = unwords [show daysAgo, "days ago"]
    | hoursAgo > 0 = unwords [show hoursAgo, "hours ago"]
    | otherwise = unwords [show minutesAgo, "minutes ago"]
  where
    minutesAgo = round $ dt / 60 :: Int
    hoursAgo = round $ dt / (60 * 60) :: Int
    daysAgo = round $ dt / (24 * 60 * 60) :: Int
    dt = diffUTCTime now t
