module Wolf.Cli.Editor where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T

import System.Environment
import System.Process

startEditorOn :: MonadIO m => Path Abs File -> m EditingResult
startEditorOn path = do
    ensureDir $ parent path
    let getContentsOfFile =
            liftIO $ forgivingAbsence $ SB.readFile $ toFilePath path
    contentsBefore <- getContentsOfFile
    meditor <- liftIO $ lookupEnv "EDITOR"
    let editor = fromMaybe "vim" meditor
    let cp = proc editor [toFilePath path]
    ec <-
        liftIO $ do
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph
    contentsAfter <- getContentsOfFile
    case ec of
        ExitFailure code ->
            pure $
            EditingFailure $
            T.pack $
            unwords
                [ "Invoking"
                , editor
                , "on"
                , toFilePath path
                , "failed with exit code"
                , show code
                ]
        ExitSuccess ->
            pure $
            if contentsBefore == contentsAfter
                then EditingFailure $
                     T.pack $
                     unwords
                         [ "Nothing was changed in file"
                         , toFilePath path
                         , "after editing it with"
                         , editor
                         ]
                else EditingSuccess

data EditingResult
    = EditingSuccess
    | EditingFailure Text
    deriving (Show, Eq)
