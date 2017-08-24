module Wolf.Server.Utils
    ( runDataForAccount
    , runDataForAccountUUID
    ) where

import Import

import Control.Monad.Reader

import Wolf.Data.Types

import Wolf.API
import Wolf.Server.Path
import Wolf.Server.Types

runDataForAccount :: Account -> ReaderT DataSettings IO a -> WolfHandler a
runDataForAccount acc = runDataForAccountUUID (accountUUID acc)

runDataForAccountUUID ::
       AccountUUID -> ReaderT DataSettings IO a -> WolfHandler a
runDataForAccountUUID uuid func = do
    dd <- accountDataDir uuid
    let ds = DataSettings {dataSetWolfDir = dd}
    liftIO $ runReaderT func ds
