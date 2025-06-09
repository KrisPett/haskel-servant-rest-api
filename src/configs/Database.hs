module Configs.Database
  ( DBConnectionString,
    connStr,
    initDB,
    initConnectionPool,
    runDB,
  ) where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Pool
import Database.Persist.Postgresql
import Database.Persist.Sql (SqlBackend)
import Entity.Models (migrateAll)
import qualified Data.ByteString.Char8 as BS

type DBConnectionString = ConnectionString

connStr :: DBConnectionString
connStr = BS.pack "host=haskel-servant-postgres dbname=haskel-servant-postgres-db user=admin password=admin"

initDB :: Pool SqlBackend -> IO ()
initDB pool = runSqlPool (runMigration migrateAll) pool

initConnectionPool :: DBConnectionString -> IO (Pool SqlBackend)
initConnectionPool connstring = runStderrLoggingT $ createPostgresqlPool connstring 10

runDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
runDB pool query = runSqlPool query pool
