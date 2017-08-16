{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Entry where

import Import

import Wolf.Data.Entry.Types

-- | Get the individual entry list out of a person entry.
personEntryTuples :: PersonEntry -> [(Text, PersonPropertyValue)]
personEntryTuples = personEntryProperties
