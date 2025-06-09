{-# LANGUAGE OverloadedStrings #-}

module Configs.Seed (seedDB) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (SelectOpt (LimitTo), SqlBackend, SqlPersistT, insert_, runSqlPool, selectList)
import Entity.Models
import Helpers.EntityHelpers (newAiGeneratedArticle, newAiNewsCategorized, newAiNewsResponse)
import Helpers.Time (parseUTC)

seedDB :: Pool SqlBackend -> IO ()
seedDB pool = runSqlPool seed pool

seed :: SqlPersistT IO ()
seed = do
  existing <- selectList [] [LimitTo 1] :: SqlPersistT IO [Entity AiGeneratedArticle]
  when (null existing) $ do
    now <- liftIO getCurrentTime
    -- AiGeneratedArticle entries
    -- Insert a new AiGeneratedArticle with alternative constructor
    insert_ $
      newAiGeneratedArticle
        now
        "The Ethics of AI"
        "Exploring moral implications of AI"
        "Ethics in AI covers bias, privacy, and accountability..."

    insert_ $
      AiGeneratedArticle
        { aiGeneratedArticleCreatedAt = parseUTC "2024-05-01 00:00:00",
          aiGeneratedArticleUpdatedAt = parseUTC "2024-05-01 00:00:00",
          aiGeneratedArticleTitle = "AI in Transportation",
          aiGeneratedArticleDescription = "Self-driving cars and beyond",
          aiGeneratedArticleContent = "AI is shaping the future of how we travel..."
        }

    -- AiNewsCategorized entries
    insert_ $
      AiNewsCategorized
        { aiNewsCategorizedCreatedAt = parseUTC "2024-06-01 00:00:00",
          aiNewsCategorizedUpdatedAt = parseUTC "2024-06-01 00:00:00",
          aiNewsCategorizedTitle = "AI and Climate Change",
          aiNewsCategorizedDescription = "Can AI help solve environmental issues?",
          aiNewsCategorizedContent = "AI models are used for climate predictions and resource optimization..."
        }

    -- AiNewsResponse entries
    insert_ $
      AiNewsResponse
        { aiNewsResponseCreatedAt = parseUTC "2024-07-01 00:00:00",
          aiNewsResponseUpdatedAt = parseUTC "2024-07-01 00:00:00",
          aiNewsResponseInputMessage = "What’s new in AI policy?",
          aiNewsResponseDate = "2024-07-01",
          aiNewsResponseLinks = 5,
          aiNewsResponseUrls = "http://policy-ai.org, http://openai.com/policy",
          aiNewsResponseText = "New regulations are being proposed to govern AI deployment..."
        }
