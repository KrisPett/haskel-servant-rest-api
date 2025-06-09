{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Services.AiGeneratedArticlesService
  ( AiGeneratedArticlesServiceI(..)
  , AiGeneratedArticlesService(..)
  , newAiGeneratedArticlesService
  , runService
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Servant (Handler)

import Configs.Database (runDB)
import Entity.Models (AiGeneratedArticle(..), AiGeneratedArticleId, EntityField(AiGeneratedArticleId))
import Helpers.EntityHelpers (newAiGeneratedArticle)
import Middlewares.ErrorHandler (AppError(..), throwAppError)

class Monad m => AiGeneratedArticlesServiceI m where
  findAll :: m [Entity AiGeneratedArticle]
  findById :: AiGeneratedArticleId -> m (Entity AiGeneratedArticle)
  insertArticle :: Text -> Text -> Text -> m (Entity AiGeneratedArticle)

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
    service <- ask
    result <- liftIO $ runDB (servicePool service) $ selectList [] []
    case result of
      articles -> return articles

  findById articleId = do
    service <- ask
    result <- liftIO $ runDB (servicePool service) $ get articleId
    case result of
      Nothing -> lift $ throwAppError $ NotFoundError $ "AiGeneratedArticle with id not found"
      Just article -> return $ Entity articleId article

  insertArticle title description content = do
    service <- ask
    now <- liftIO getCurrentTime
    let newArticle = newAiGeneratedArticle now title description content
    result <- liftIO $ runDB (servicePool service) $ insert newArticle
    case result of
      articleId -> return $ Entity articleId newArticle