{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cli.Command.Review where

import Import

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Cli.OptParse.Types
import Wolf.Cli.Utils
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.NoteIndex
import Wolf.Data.Report
import Wolf.Data.Time
import Wolf.Data.Types

review :: (MonadIO m, MonadReader Settings m) => m ()
review =
    runData $
    withInitCheck $ do
        index <- getIndexWithDefault
        let tups =
                nubBy (\t1 t2 -> snd t1 == snd t2) $ M.toList $ indexMap index
        noteTups <-
            fmap concat $
            forM tups $ \(nn, personUuid) -> do
                notes <- getPersonNotes personUuid
                pure $ map ((,) nn) notes
        let entries = sortOn (personNoteTimestamp . snd) noteTups
        now <- liftIO getCurrentTime
        let report =
                mconcat $
                flip map entries $ \(nickName, personNote) ->
                    unlinesReport
                        [ colored [SetColor Foreground Dull Blue] $
                          unwords
                              [ T.unpack nickName ++ ","
                              , T.unpack $
                                formatMomentNicely
                                    now
                                    (personNoteTimestamp personNote)
                              ]
                        , stringReport $
                          T.unpack $ personNoteContents personNote
                        ]
        liftIO $ putStr $ renderReport report
