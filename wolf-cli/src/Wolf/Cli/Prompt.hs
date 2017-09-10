module Wolf.Cli.Prompt
    ( promptYesNo
    , YesNo(..)
    , promptRawString
    ) where

import Import

import System.IO (hFlush, stdout)

promptYesNo :: YesNo -> String -> IO YesNo
promptYesNo def p = do
    let defaultString =
            case def of
                Yes -> "[Y/n]"
                No -> "[y/N]"
    rs <- promptRawString $ p ++ " " ++ defaultString
    case rs of
        "yes" -> pure Yes
        "y" -> pure Yes
        "no" -> pure No
        "n" -> pure No
        _ -> pure def

data YesNo
    = Yes
    | No
    deriving (Show, Eq)

promptRawString :: String -> IO String
promptRawString s = do
    putStr $ s ++ " > "
    hFlush stdout
    getLine
