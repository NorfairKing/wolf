{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Index.Types.Gen where

import Import

import Wolf.Data.Index.Types

import Wolf.Data.People.Types.Gen ()

instance GenUnchecked Index

instance GenValid Index where
    genValid = Index <$> genValid
