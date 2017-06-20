module TestUtils where

import TestImport

withSandbox :: SpecWith (Path Abs Dir) -> Spec
withSandbox func =
    beforeAll
        (do sandboxDir <- resolveDir' "sandbox"
            pure sandboxDir) $
    afterAll (ignoringAbsence . removeDirRecur) func
