module TestUtils where

import TestImport

withSandbox :: SpecWith (Path Abs Dir) -> Spec
withSandbox func =
    beforeAll
        (do sandboxDir <- resolveDir' "sandbox"
            ignoringAbsence $ removeDirRecur sandboxDir
            pure sandboxDir) $
    afterAll (ignoringAbsence . removeDirRecur) func
