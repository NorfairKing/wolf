{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Report
    ( Report
    , renderReport
    , stringReport
    , colored
    , unlinesReport
    ) where

import Import

import System.Console.ANSI as ANSI

newtype Report =
    Report [([ANSI.SGR], String)]
    deriving (Show, Eq, Generic)

instance Monoid Report where
    mempty = Report []
    mappend (Report r1) (Report r2) = Report $ r1 ++ r2

instance IsString Report where
    fromString s = Report [([], s)]

renderReport :: Report -> String
renderReport (Report tups) =
    concatMap (\(cmds, content) -> ANSI.setSGRCode cmds <> content) tups

stringReport :: String -> Report
stringReport = fromString

colored :: [ANSI.SGR] -> String -> Report
colored cmds s = Report [(cmds, s)]

unlinesReport :: [Report] -> Report
unlinesReport rs = mconcat $ map (<> "\n") rs
