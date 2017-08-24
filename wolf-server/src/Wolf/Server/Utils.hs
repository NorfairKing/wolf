module Wolf.Server.Utils (runDataForAccount, runDataForAccountUUID )where

import Import

import Control.Monad.Reader

import Wolf.Data.Types

import Wolf.Server.Path
import Wolf.Server.Types
import Wolf.API

runDataForAccount :: Account -> ReaderT DataSettings IO a -> WolfHandler a
runDataForAccount acc func = runDataForAccountUUID (accountUUID acc) func
runDataForAccountUUID :: AccountUUID -> ReaderT DataSettings IO a -> WolfHandler a
runDataForAccountUUID uuid func = do
    dd <- accountDataDir uuid
    let ds = DataSettings {dataSetWolfDir = dd}
    liftIO $ runReaderT func ds
