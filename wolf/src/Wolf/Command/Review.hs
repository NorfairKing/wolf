{-# LANGUAGE FlexibleContexts #-}

module Wolf.Command.Review where

import Import

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import System.Console.ANSI as ANSI

import Wolf.Index
import Wolf.Init
import Wolf.NoteIndex
import Wolf.OptParse.Types
import Wolf.Report
import Wolf.Time
import Wolf.Types

review :: (MonadIO m, MonadReader Settings m) => m ()
review =
    withInitCheck $ do
        index <- getIndex
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
                              [ nickName ++ ","
                              , formatMomentNicely
                                    now
                                    (personNoteTimestamp personNote)
                              ]
                        , stringReport $
                          T.unpack $ personNoteContents personNote
                        ]
        liftIO $ putStr $ renderReport report
