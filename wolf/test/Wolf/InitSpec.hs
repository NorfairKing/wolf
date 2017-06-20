module Wolf.InitSpec
    ( spec
    ) where

import TestImport
import TestUtils

import Wolf.Init

spec :: Spec
spec =
    withSandbox $
    describe "init" $
    it "does not fail in a sandbox directory" $ \sb -> initIn sb :: IO ()
