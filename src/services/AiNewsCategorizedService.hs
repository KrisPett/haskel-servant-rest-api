
-- AiNewsCategorizedService.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Services.AiNewsCategorizedService
  ( AiNewsCategorizedServiceI (..),
    AiNewsCategorizedService (..),
    newAiNewsCategorizedService,
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
import Entity.Models (AiNewsCategorized (..), AiNewsCategorizedId)
import Helpers.EntityHelpers (newAiNewsCategorized)
import Middlewares.ErrorHandler (AppError (..), throwAppError)
import Servant (Handler)

class (Monad m) => AiNewsCategorizedServiceI m where
  findAll :: m [Entity AiNewsCategorized]
  findById :: AiNewsCategorizedId -> m (Entity AiNewsCategorized)
  insertCategorized :: Text -> Text -> Text -> m (Entity AiNewsCategorized)

newtype AiNewsCategorizedService = AiNewsCategorizedService
  { servicePool :: Pool SqlBackend
  }

newAiNewsCategorizedService :: Pool SqlBackend -> AiNewsCategorizedService
newAiNewsCategorizedService = AiNewsCategorizedService

type ServiceM = ReaderT AiNewsCategorizedService Handler

runService :: AiNewsCategorizedService -> ServiceM a -> Handler a
runService service action = runReaderT action service

instance AiNewsCategorizedServiceI ServiceM where
  findAll = do
    pool <- asks servicePool
    result <- liftIO $ runDB pool $ selectList [] []
    liftIO $ putStrLn "Running AiNewsCategorizedServiceI and fetching articles..."
    pure result

  findById recordId = do
    pool <- asks servicePool
    result <- liftIO $ runDB pool $ get recordId
    case result of
      Nothing -> lift $ throwAppError $ NotFoundError "AiNewsCategorized with id not found"
      Just categorized -> pure $ Entity recordId categorized

  insertCategorized title description content = do
    pool <- asks servicePool
    now <- liftIO getCurrentTime
    let newCategorized = newAiNewsCategorized now title description content
    recordId <- liftIO $ runDB pool $ insert newCategorized
    pure $ Entity recordId newCategorized