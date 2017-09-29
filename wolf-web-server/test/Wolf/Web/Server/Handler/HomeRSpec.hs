module Wolf.Web.Server.Handler.HomeRSpec where

import TestImport

import Yesod.Test

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerSpec $
    ydescribe "HomeR" $
    yit "returns a 200" $ do
        get HomeR
        statusIs 200
