{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.HomePage
  ( PageAPI,
    pageServer,
  )
where

import Configs.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Entity.Models (EntityField (..), Message (..), MessageId)
import Servant

type PageAPI =
  "home" :> Get '[PlainText] String
    :<|> "createNewsPage" :> Get '[PlainText] String
    :<|> "messages" :> Get '[JSON] [Entity Message]
    :<|> "messages" :> ReqBody '[JSON] Text :> Post '[JSON] (Entity Message)

pageServer :: Pool SqlBackend -> Server PageAPI
pageServer pool = homeHandler :<|> createNewsPageHandler :<|> getAllMessages :<|> createMessage
  where
    homeHandler :: Handler String
    homeHandler = return "Welcome to the Home Page!"

    createNewsPageHandler :: Handler String
    createNewsPageHandler = return "This is the Create News Page!"

    getAllMessages :: Handler [Entity Message]
    getAllMessages = liftIO $ runDB pool $ selectList [] []

    createMessage :: Text -> Handler (Entity Message)
    createMessage content = do
      let msg = Message content
      msgId <- liftIO $ runDB pool $ insert msg
      return $ Entity msgId msg
