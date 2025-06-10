{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.AiGeneratedArticlesAPI
  ( AiGeneratedArticlesAPI,
    aiGeneratedArticlesServer,
    AiGeneratedArticleRequest (..),
    AiGeneratedArticleResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), Key)
import Entity.Models (AiGeneratedArticle (..), AiGeneratedArticleId)
import GHC.Generics (Generic)
import Servant
import Services.AiGeneratedArticlesService

data AiGeneratedArticleRequest = AiGeneratedArticleRequest
  { reqTitle :: Text,
    reqDescription :: Text,
    reqContent :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AiGeneratedArticleRequest

instance ToJSON AiGeneratedArticleRequest

data AiGeneratedArticleResponse = AiGeneratedArticleResponse
  { respId :: Text,
    respTitle :: Text,
    respDescription :: Text,
    respContent :: Text,
    respCreatedAt :: Text,
    respUpdatedAt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AiGeneratedArticleResponse

instance ToJSON AiGeneratedArticleResponse

entityToResponse :: Entity AiGeneratedArticle -> AiGeneratedArticleResponse
entityToResponse (Entity articleId article) =
  AiGeneratedArticleResponse
    { respId = T.pack $ show articleId,
      respTitle = aiGeneratedArticleTitle article,
      respDescription = aiGeneratedArticleDescription article,
      respContent = aiGeneratedArticleContent article,
      respCreatedAt = T.pack $ show $ aiGeneratedArticleCreatedAt article,
      respUpdatedAt = T.pack $ show $ aiGeneratedArticleUpdatedAt article
    }

type AiGeneratedArticlesAPI =
  "ai-articles" :> Get '[JSON] [AiGeneratedArticleResponse]
    :<|> "ai-articles" :> Capture "id" AiGeneratedArticleId :> Get '[JSON] AiGeneratedArticleResponse
    :<|> "ai-articles" :> ReqBody '[JSON] AiGeneratedArticleRequest :> Post '[JSON] AiGeneratedArticleResponse

aiGeneratedArticlesServer :: AiGeneratedArticlesService -> Server AiGeneratedArticlesAPI
aiGeneratedArticlesServer service =
  getAllArticles
    :<|> getArticleById
    :<|> createArticle
  where
    getAllArticles :: Handler [AiGeneratedArticleResponse]
    getAllArticles = do
      articles <- runService service findAll
      return $ map entityToResponse articles

    getArticleById :: AiGeneratedArticleId -> Handler AiGeneratedArticleResponse
    getArticleById articleId = do
      article <- runService service (findById articleId)
      return $ entityToResponse article
      
    createArticle :: AiGeneratedArticleRequest -> Handler AiGeneratedArticleResponse
    createArticle req = do
      article <-
        runService service $
          insertArticle
            (reqTitle req)
            (reqDescription req)
            (reqContent req)
      return $ entityToResponse article
