module Wolf.Web.Server.Handler.HomeRSpec where

import TestImport

import Yesod.Test

import Wolf.Web.Server.Foundation
import Wolf.Web.Server.TestUtils

spec :: Spec
spec =
    wolfWebServerPersonalSpec $
    ydescribe "HomeR" $
    yit "gets a 200 for an example user" $
    withExampleAccount_ $ do
        get HomeR
        statusIs 200
