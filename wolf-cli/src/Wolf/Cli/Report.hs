{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Report
    ( Report
    , renderReport
    , renderReportString
    , stringReport
    , textReport
    , colored
    , coloredString
    , unlinesReport
    , putReport
    , putReportLn
    ) where

import Import

import qualified Data.Text as T

import System.Console.ANSI as ANSI

newtype Report =
    Report [([ANSI.SGR], String)]
    deriving (Show, Eq, Generic)

instance Monoid Report where
    mempty = Report []
    mappend (Report r1) (Report r2) = Report $ r1 ++ r2

instance IsString Report where
    fromString s = Report [([], s)]

renderReport :: Report -> Text
renderReport = T.pack . renderReportString

renderReportString :: Report -> String
renderReportString (Report tups) =
    concatMap (\(cmds, content) -> ANSI.setSGRCode cmds <> content) tups

stringReport :: String -> Report
stringReport = fromString

textReport :: Text -> Report
textReport = stringReport . T.unpack

colored :: [ANSI.SGR] -> Text -> Report
colored cmds s = Report [(cmds, T.unpack s)]

coloredString :: [ANSI.SGR] -> String -> Report
coloredString cmds s = Report [(cmds, s)]

unlinesReport :: [Report] -> Report
unlinesReport rs = mconcat $ intersperse "\n" rs

putReport :: Report -> IO ()
putReport = putStr . renderReportString

putReportLn :: Report -> IO ()
putReportLn = putStrLn . renderReportString
