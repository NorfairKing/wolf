{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Export.Types.Gen where

import Import

import Wolf.Data.Export.Types

import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.Init.Types.Gen ()

instance GenUnchecked Export

instance GenValid Export where
    genValid = Export <$> genValid <*> genValid
