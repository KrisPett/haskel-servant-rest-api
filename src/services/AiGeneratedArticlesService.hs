{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Services.AiGeneratedArticlesService
  ( AiGeneratedArticlesServiceI (..),
    AiGeneratedArticlesService (..),
    newAiGeneratedArticlesService,
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
import Entity.Models (AiGeneratedArticle (..), AiGeneratedArticleId)
import Helpers.EntityHelpers (newAiGeneratedArticle)
import Middlewares.ErrorHandler (AppError (..), throwAppError)
import Servant (Handler)

class (Monad m) => AiGeneratedArticlesServiceI m where
  findAll :: m [Entity AiGeneratedArticle]
  findById :: AiGeneratedArticleId -> m (Entity AiGeneratedArticle)
  insertArticle :: Text -> Maybe Text -> Text -> m (Entity AiGeneratedArticle)

newtype AiGeneratedArticlesService = AiGeneratedArticlesService
  { servicePool :: Pool SqlBackend
  }

newAiGeneratedArticlesService :: Pool SqlBackend -> AiGeneratedArticlesService
newAiGeneratedArticlesService = AiGeneratedArticlesService

type ServiceM = ReaderT AiGeneratedArticlesService Handler

runService :: AiGeneratedArticlesService -> ServiceM a -> Handler a
runService service action = runReaderT action service

instance AiGeneratedArticlesServiceI ServiceM where
  findAll = do
    pool <- asks servicePool
    liftIO $ runDB pool $ selectList [] []

  findById articleId = do
    pool <- asks servicePool
    result <- liftIO $ runDB pool $ get articleId
    case result of
      Nothing -> lift $ throwAppError $ NotFoundError "AiGeneratedArticle with id not found"
      Just article -> pure $ Entity articleId article

  insertArticle title description content = do
    pool <- asks servicePool
    now <- liftIO getCurrentTime
    let newArticle = newAiGeneratedArticle now title description content
    articleId <- liftIO $ runDB pool $ insert newArticle
    pure $ Entity articleId newArticle