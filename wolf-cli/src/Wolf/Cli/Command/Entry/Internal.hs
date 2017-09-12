{-# LANGUAGE FlexibleContexts #-}

module Wolf.Cli.Command.Entry.Internal where

import Import

import qualified Data.Text as T

import Wolf.Data

parseFirstnameLastname :: Alias -> Maybe (Text, Text)
parseFirstnameLastname s =
    case T.words $ aliasText s of
        [fn, ln] -> Just (fn, ln)
        _ -> Nothing

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhite . reverse . dropWhite
  where
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

tmpPersonEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
tmpPersonEntryFile personUuid = do
    td <- liftIO getTempDir
    liftIO $
        resolveFile td $ T.unpack (personUuidText personUuid) ++ "-entry.yaml"
