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
import Servant
import Services.AiGeneratedArticlesService (AiGeneratedArticlesService, AiGeneratedArticlesServiceI (findAll), runService)
import Services.AiNewsCategorizedService (AiNewsCategorizedService, AiNewsCategorizedServiceI (findAll), runService)

data RecentNewsSearchResultsDTO = RecentNewsSearchResultsDTO
  { rnsrId :: Text,
    rnsrTitle :: Text,
    rnsrDescription :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON RecentNewsSearchResultsDTO where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \f -> case f of
            "rnsrId" -> "id"
            "rnsrTitle" -> "title"
            "rnsrDescription" -> "description"
            other -> other
        }

instance FromJSON RecentNewsSearchResultsDTO where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \f -> case f of
            "rnsrId" -> "id"
            "rnsrTitle" -> "title"
            "rnsrDescription" -> "description"
            other -> other
        }

data AIGeneratedArticlesDTO = AIGeneratedArticlesDTO
  { agaId :: Text,
    agaTitle :: Text,
    agaDescription :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AIGeneratedArticlesDTO where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \f -> case f of
            "agaId" -> "id"
            "agaTitle" -> "title"
            "agaDescription" -> "description"
            other -> other
        }

instance FromJSON AIGeneratedArticlesDTO where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \f -> case f of
            "agaId" -> "id"
            "agaTitle" -> "title"
            "agaDescription" -> "description"
            other -> other
        }

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
  Pool SqlBackend ->
  AiNewsCategorizedService ->
  AiGeneratedArticlesService ->
  Server CreateNewsPageAPI
createNewsPageServer _ categorizedService generatedService = getCreateNewsPageHandler
  where
    getCreateNewsPageHandler :: Text -> Handler CreateNewsPageDTO
    getCreateNewsPageHandler selectedDate = do
      liftIO $ putStrLn $ "get_create_news_page - selected_date: " ++ T.unpack selectedDate
      recentNews <- Services.AiNewsCategorizedService.runService categorizedService Services.AiNewsCategorizedService.findAll
      aiArticles <- Services.AiGeneratedArticlesService.runService generatedService Services.AiGeneratedArticlesService.findAll
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
      agaDescription = aiGeneratedArticleDescription entity
    }
