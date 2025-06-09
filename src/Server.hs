{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( startApp,
    app,
  )
where

import Configs.Database
import Control.Monad.IO.Class (liftIO)
import Data.Pool
import Database.Persist.Sql (SqlBackend)
import Network.Wai.Handler.Warp
import Routes.HomePage
import Servant

type API = PageAPI

api :: Proxy API
api = Proxy

server :: Pool SqlBackend -> Server API
server pool = pageServer pool

startApp :: IO ()
startApp = do
  putStrLn "Starting server on port 8080..."
  putStrLn "Connecting to database..."
  
  pool <- initConnectionPool connStr
  initDB pool
  
  putStrLn "Database initialized successfully"
  run 8080 (app pool)

app :: Pool SqlBackend -> Application
app pool = serve api (server pool)