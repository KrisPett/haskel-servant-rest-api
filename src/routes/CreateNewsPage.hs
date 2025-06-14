{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.CreateNewsPage
  ( CreateNewsPageAPI,
    createNewsPageServer,
    CreateNewsPageDTO (..),
    RecentNewsSearchResultsDTO (..),
    AIGeneratedArticlesDTO (..),
  )
where

import Control.Concurrent.Async (async, concurrently, wait)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey)
import Entity.Models
  ( AiGeneratedArticle (..),
    AiNewsCategorized (..),
  )
import GHC.Generics (Generic)
import Helpers.Time (parseUrls)
import Helpers.Handler (unwrapServiceResult)
import Servant
import Services.AiGeneratedArticlesService (AiGeneratedArticlesService, AiGeneratedArticlesServiceI (findAll), runService)
import Services.AiNewsCategorizedService (AiNewsCategorizedService, AiNewsCategorizedServiceI (findAll), runService)

recentNewsOptions :: Options
recentNewsOptions =
  defaultOptions
    { fieldLabelModifier = \f -> case f of
        "rnsrId" -> "id"
        "rnsrTitle" -> "title"
        "rnsrDescription" -> "description"
        other -> other
    }

aiArticlesOptions :: Options
aiArticlesOptions =
  defaultOptions
    { fieldLabelModifier = \f -> case f of
        "agaId" -> "id"
        "agaTitle" -> "title"
        "agaDescription" -> "description"
        "updatedAt" -> "updatedAt"
        other -> other
    }

data RecentNewsSearchResultsDTO = RecentNewsSearchResultsDTO
  { rnsrId :: Text,
    rnsrTitle :: Text,
    rnsrDescription :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON RecentNewsSearchResultsDTO where
  toJSON = genericToJSON recentNewsOptions

instance FromJSON RecentNewsSearchResultsDTO where
  parseJSON = genericParseJSON recentNewsOptions

data AIGeneratedArticlesDTO = AIGeneratedArticlesDTO
  { agaId :: Text,
    agaTitle :: Text,
    agaDescription :: Maybe Text,
    updatedAt :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AIGeneratedArticlesDTO where
  toJSON = genericToJSON aiArticlesOptions

instance FromJSON AIGeneratedArticlesDTO where
  parseJSON = genericParseJSON aiArticlesOptions

data CreateNewsPageDTO = CreateNewsPageDTO
  { inputDateFieldOption :: Text,
    inputDateFieldOptions :: [Text],
    recentNewsSearchResults :: [RecentNewsSearchResultsDTO],
    aiGeneratedArticles :: [AIGeneratedArticlesDTO]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DTOBuilder = DTOBuilder
  { builderInputDateFieldOption :: Text,
    builderInputDateFieldOptions :: [Text],
    builderRecentNewsSearchResults :: [Entity AiNewsCategorized],
    builderAIGeneratedArticles :: [Entity AiGeneratedArticle]
  }
  deriving (Show, Eq, Generic)

type CreateNewsPageAPI = "create_news_page" :> QueryParam' '[Required, Strict] "selected_date" Text :> Get '[JSON] CreateNewsPageDTO

createNewsPageServer ::
  AiNewsCategorizedService ->
  AiGeneratedArticlesService ->
  Server CreateNewsPageAPI
createNewsPageServer categorizedService generatedService = getCreateNewsPageHandler
  where
    getCreateNewsPageHandler :: Text -> Handler CreateNewsPageDTO
    getCreateNewsPageHandler selectedDate =
      handler `catchError` \e -> do
        liftIO $ putStrLn $ "get_create_news_page failed: " ++ show e
        throwError e
      where
        handler = do
          liftIO $ putStrLn $ "get_create_news_page - selected_date: " ++ T.unpack selectedDate

          (aiArticles, recentNews) <-
            liftIO $
              concurrently
                (unwrapServiceResult  $ fmap (Right :: [Entity AiGeneratedArticle] -> Either String [Entity AiGeneratedArticle]) (Services.AiGeneratedArticlesService.runService generatedService Services.AiGeneratedArticlesService.findAll))
                (unwrapServiceResult  $ fmap (Right :: [Entity AiNewsCategorized] -> Either String [Entity AiNewsCategorized]) (Services.AiNewsCategorizedService.runService categorizedService Services.AiNewsCategorizedService.findAll))

          let dtoBuilder =
                DTOBuilder
                  { builderInputDateFieldOption = selectedDate,
                    builderInputDateFieldOptions = parseUrls selectedDate,
                    builderRecentNewsSearchResults = recentNews,
                    builderAIGeneratedArticles = aiArticles
                  }
          pure $ toCreateNewsPageDTO dtoBuilder

toCreateNewsPageDTO :: DTOBuilder -> CreateNewsPageDTO
toCreateNewsPageDTO dtoBuilder =
  CreateNewsPageDTO
    { inputDateFieldOption = builderInputDateFieldOption dtoBuilder,
      inputDateFieldOptions = builderInputDateFieldOptions dtoBuilder,
      recentNewsSearchResults = map toRecentNewsDTO (builderRecentNewsSearchResults dtoBuilder),
      aiGeneratedArticles = map toAIGeneratedDTO (builderAIGeneratedArticles dtoBuilder)
    }

toRecentNewsDTO :: Entity AiNewsCategorized -> RecentNewsSearchResultsDTO
toRecentNewsDTO (Entity entityId entity) =
  RecentNewsSearchResultsDTO
    { rnsrId = T.pack . show $ fromSqlKey entityId,
      rnsrTitle = aiNewsCategorizedTitle entity,
      rnsrDescription = aiNewsCategorizedDescription entity
    }

toAIGeneratedDTO :: Entity AiGeneratedArticle -> AIGeneratedArticlesDTO
toAIGeneratedDTO (Entity entityId entity) =
  AIGeneratedArticlesDTO
    { agaId = T.pack . show $ fromSqlKey entityId,
      agaTitle = aiGeneratedArticleTitle entity,
      agaDescription = aiGeneratedArticleDescription entity,
      updatedAt = T.pack . show $ aiGeneratedArticleUpdatedAt entity
    }
