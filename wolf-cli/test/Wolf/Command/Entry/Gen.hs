{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Command.Entry.Gen where

import TestImport

import Wolf.Cli.Command.Entry.Internal

instance GenUnchecked RawYaml

instance GenValid RawYaml
