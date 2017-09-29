module Wolf.Web.Server.Handler.Person.NewNoteRSpec where

import TestImport

import Yesod.Test

import Wolf.Data

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerSpec $
    ydescribe "PersonNewNoteRSpec" $
    yit "returns a 200 for each person" $ do
        uuids <- runTestData getPersonUuids
        -- TODO actually put some data in here.
        forM_ uuids $ \uuid -> do
            get $ PersonNewNoteR uuid
            statusIs 200
