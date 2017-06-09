module Wolf.Summary where

import Import

import qualified Data.Map as M

import Wolf.Index
import Wolf.NoteIndex
import Wolf.Types

summary :: String -> IO ()
summary person = do
    index <- getIndex
    case lookupInIndex person index of
        Nothing -> die $ unwords ["No person found for", show person]
        Just personUuid -> do
            mpe <- getPersonEntry personUuid
            pns <- getPersonNotes personUuid
            putStr $ summaryReport mpe pns

summaryReport :: Maybe PersonEntry -> [PersonNote] -> String
summaryReport mpe pns =
    unlines $
    [ case mpe of
          Nothing -> "No person entry."
          Just pe ->
              unlines $
              flip map (M.toList $ personEntryProperties pe) $ \(prop, val) ->
                  unwords [prop ++ ":", val]
    , ""
    ] ++
    map show pns
