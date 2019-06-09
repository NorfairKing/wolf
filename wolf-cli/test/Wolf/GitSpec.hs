module Wolf.GitSpec
  ( spec
  ) where

import TestImport
import TestUtils

import Wolf.Data

import Wolf.Cli.Command.Git
import Wolf.Cli.OptParse.Types

import Wolf.Data.Gen ()

spec :: Spec
spec =
  describe "git" $
  withSandbox $
  it "fails if no wolf repo has been initialised" $ \sb ->
    once $
    forAllValid $ \args ->
      runReaderT
        (git args)
        Settings {setDataSets = DataSettings {dataSetWolfDir = sb, dataSetGitExecutable = Nothing}} `shouldThrow`
      (\e -> e == ExitFailure 1)
