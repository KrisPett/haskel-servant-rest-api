{-# LANGUAGE OverloadedStrings #-}

module Helpers.EntityHelpers
  ( newAiGeneratedArticle
  , newAiNewsCategorized
  , newAiNewsResponse
  ) where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Entity.Models (AiGeneratedArticle(..), AiNewsCategorized(..), AiNewsResponse(..))

-- AiGeneratedArticle constructor
newAiGeneratedArticle :: UTCTime -> Text -> Maybe Text -> Text -> AiGeneratedArticle
newAiGeneratedArticle now title desc content = AiGeneratedArticle
  { aiGeneratedArticleCreatedAt = now
  , aiGeneratedArticleUpdatedAt = now
  , aiGeneratedArticleTitle = title
  , aiGeneratedArticleDescription = desc
  , aiGeneratedArticleContent = content
  }

-- AiNewsCategorized constructor
newAiNewsCategorized :: UTCTime -> Text -> Text -> Text -> AiNewsCategorized
newAiNewsCategorized now title desc content = AiNewsCategorized
  { aiNewsCategorizedCreatedAt = now
  , aiNewsCategorizedUpdatedAt = now
  , aiNewsCategorizedTitle = title
  , aiNewsCategorizedDescription = desc
  , aiNewsCategorizedContent = content
  }

-- AiNewsResponse constructor
newAiNewsResponse :: UTCTime -> Text -> Text -> Int -> Text -> Text -> AiNewsResponse
newAiNewsResponse now inputMsg date links urls text = AiNewsResponse
  { aiNewsResponseCreatedAt = now
  , aiNewsResponseUpdatedAt = now
  , aiNewsResponseInputMessage = inputMsg
  , aiNewsResponseDate = date
  , aiNewsResponseLinks = links
  , aiNewsResponseUrls = urls
  , aiNewsResponseText = text
  }