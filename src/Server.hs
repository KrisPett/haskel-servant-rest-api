{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( startApp,
    app,
  )
where

import Configs.Database
import Configs.Seed (seedDB)
import Data.Pool
import Database.Persist.Sql (SqlBackend)
import Network.Wai.Handler.Warp
import Routes.AiGeneratedArticlesAPI
import Routes.CreateNewsPage
import Routes.HomePage
import Servant
import Services.AiGeneratedArticlesService
import Services.AiNewsResponseService
import Services.AiNewsCategorizedService

type API =
  HomePageAPI
    :<|> AiGeneratedArticlesAPI
    :<|> CreateNewsPageAPI

api :: Proxy API
api = Proxy

server :: Pool SqlBackend -> Server API
server pool =
  let aiService = newAiGeneratedArticlesService pool
      aiNewsResponseService = newAiNewsResponseService pool
      aiNewsCategorizedService = newAiNewsCategorizedService pool
   in homePageServer pool aiNewsResponseService
        :<|> aiGeneratedArticlesServer aiService
        :<|> createNewsPageServer pool aiNewsCategorizedService aiService

startApp :: IO ()
startApp = do
  putStrLn "Starting server on port 8080..."
  putStrLn "Connecting to database..."

  pool <- initConnectionPool connStr
  initDB pool
  putStrLn "Database initialized successfully"
  seedDB pool
  putStrLn "Database seeded with initial data"
  run 8080 (app pool)

app :: Pool SqlBackend -> Application
app pool = serve api (server pool)