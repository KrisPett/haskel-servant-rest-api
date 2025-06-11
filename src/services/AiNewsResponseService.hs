{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Services.AiNewsResponseService
  ( AiNewsResponseServiceI (..),
    AiNewsResponseService (..),
    newAiNewsResponseService,
    runService,
  )
where

import Configs.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Entity.Models (AiNewsResponse (..), AiNewsResponseId)
import Helpers.EntityHelpers (newAiNewsResponse)
import Middlewares.ErrorHandler (AppError (..), throwAppError)
import Servant (Handler)

class (Monad m) => AiNewsResponseServiceI m where
  findAll :: m [Entity AiNewsResponse]
  findById :: AiNewsResponseId -> m (Entity AiNewsResponse)
  insertResponse :: Text -> Text -> Int -> Text -> Text -> m (Entity AiNewsResponse)

newtype AiNewsResponseService = AiNewsResponseService
  { servicePool :: Pool SqlBackend
  }

newAiNewsResponseService :: Pool SqlBackend -> AiNewsResponseService
newAiNewsResponseService = AiNewsResponseService

type ServiceM = ReaderT AiNewsResponseService Handler

runService :: AiNewsResponseService -> ServiceM a -> Handler a
runService service action = runReaderT action service

instance AiNewsResponseServiceI ServiceM where
  findAll = do
    pool <- asks servicePool
    liftIO $ runDB pool $ selectList [] []

  findById recordId = do
    pool <- asks servicePool
    result <- liftIO $ runDB pool $ get recordId
    case result of
      Nothing -> lift $ throwAppError $ NotFoundError "AiNewsResponse with id not found"
      Just response -> pure $ Entity recordId response

  insertResponse inputMessage date links urls text = do
    pool <- asks servicePool
    now <- liftIO getCurrentTime
    let newResponse = newAiNewsResponse now inputMessage date links urls text
    recordId <- liftIO $ runDB pool $ insert newResponse
    pure $ Entity recordId newResponse