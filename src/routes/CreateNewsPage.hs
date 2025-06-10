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
import GHC.Generics (Generic)
import Servant
import Entity.Models
  ( AiGeneratedArticle (..),
    AiNewsCategorized (..),
  )
import Services.AiGeneratedArticlesService (AiGeneratedArticlesService, AiGeneratedArticlesServiceI(findAll), runService)
import Services.AiNewsCategorizedService (AiNewsCategorizedService, AiNewsCategorizedServiceI(findAll), runService)
-- import Services.CreateNewsPageService (getAIGeneratedArticles, getRecentNewsSearchResults)

-- | DTOs
data RecentNewsSearchResultsDTO = RecentNewsSearchResultsDTO
  { aiNewsCategorizedId :: Text,
    recentNewsTitle :: Text,
    recentNewsDescription :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AIGeneratedArticlesDTO = AIGeneratedArticlesDTO
  { aiGeneratedArticleId :: Text,
    aiGeneratedTitle :: Text,
    aiGeneratedDescription :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateNewsPageDTO = CreateNewsPageDTO
  { inputDateFieldOption :: Text,
    inputDateFieldOptions :: [Text],
    recentNewsSearchResults :: [RecentNewsSearchResultsDTO],
    aiGeneratedArticles :: [AIGeneratedArticlesDTO]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | API Endpoint
type CreateNewsPageAPI = "create_news_page" :> QueryParam' '[Required, Strict] "selected_date" Text :> Get '[JSON] CreateNewsPageDTO

createNewsPageServer
  :: Pool SqlBackend
  -> AiNewsCategorizedService
  -> AiGeneratedArticlesService
  -> Server CreateNewsPageAPI
createNewsPageServer _ categorizedService generatedService = getCreateNewsPageHandler
  where
    getCreateNewsPageHandler :: Text -> Handler CreateNewsPageDTO
    getCreateNewsPageHandler selectedDate = do
      liftIO $ putStrLn $ "get_create_news_page - selected_date: " ++ T.unpack selectedDate
      recentNews <- Services.AiNewsCategorizedService.runService categorizedService Services.AiNewsCategorizedService.findAll
      aiArticles <- Services.AiGeneratedArticlesService.runService generatedService Services.AiGeneratedArticlesService.findAll
      let dto =
            CreateNewsPageDTO
              { inputDateFieldOption = selectedDate,
                inputDateFieldOptions = [selectedDate],
                recentNewsSearchResults = map toRecentNewsDTO recentNews,
                aiGeneratedArticles = map toAIGeneratedDTO aiArticles
              }
      return dto

-- | DTO Transformers

toRecentNewsDTO :: Entity AiNewsCategorized -> RecentNewsSearchResultsDTO
toRecentNewsDTO (Entity eid entity) =
  RecentNewsSearchResultsDTO
    { aiNewsCategorizedId = T.pack . show $ fromSqlKey eid,
      recentNewsTitle = aiNewsCategorizedTitle entity,
      recentNewsDescription = aiNewsCategorizedDescription entity
    }

toAIGeneratedDTO :: Entity AiGeneratedArticle -> AIGeneratedArticlesDTO
toAIGeneratedDTO (Entity eid entity) =
  AIGeneratedArticlesDTO
    { aiGeneratedArticleId = T.pack . show $ fromSqlKey eid,
      aiGeneratedTitle = aiGeneratedArticleTitle entity,
      aiGeneratedDescription = aiGeneratedArticleDescription entity
    }
