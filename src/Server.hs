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
import Services.AiNewsCategorizedService
import Services.AiNewsResponseService

type API =
  AiGeneratedArticlesAPI
    :<|> HomePageAPI
    :<|> CreateNewsPageAPI

api :: Proxy API
api = Proxy

data AppContext = AppContext
  { dbPool :: Pool SqlBackend,
    aiService :: AiGeneratedArticlesService,
    aiNewsResponseService :: AiNewsResponseService,
    aiNewsCategorizedService :: AiNewsCategorizedService
  }

mkAppContext :: Pool SqlBackend -> AppContext
mkAppContext pool =
  AppContext
    { dbPool = pool,
      aiService = newAiGeneratedArticlesService pool,
      aiNewsResponseService = newAiNewsResponseService pool,
      aiNewsCategorizedService = newAiNewsCategorizedService pool
    }

server :: AppContext -> Server API
server ctx =
  aiGeneratedArticlesServer (aiService ctx)
    :<|> homePageServer (dbPool ctx) (aiNewsResponseService ctx)
    :<|> createNewsPageServer
      (aiNewsCategorizedService ctx)
      (aiService ctx)

startApp :: IO ()
startApp = do
  putStrLn "Starting server on port 8080..."
  putStrLn "Connecting to database..."

  pool <- initConnectionPool connStr
  initDB pool
  putStrLn "Database initialized successfully"
  seedDB pool
  putStrLn "Database seeded with initial data"
  run 8080 $ app pool

app :: Pool SqlBackend -> Application
app pool = serve api $ server $ mkAppContext pool
