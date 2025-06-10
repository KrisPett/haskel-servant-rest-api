{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.HomePage
  ( PageAPI,
    pageServer,
    QueryResponseDTO (..),
    HomePageDTO (..),
  )
where

import Configs.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    withObject,
    (.:),
    defaultOptions,
    Options (fieldLabelModifier),
    genericToJSON,
    genericParseJSON,
  )
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey) -- Import fromSqlKey
import Entity.Models (AiNewsResponse (..), EntityField (..), Message (..), MessageId)
import GHC.Generics (Generic)
import Servant
import Services.AiNewsResponseService (AiNewsResponseService, AiNewsResponseServiceI (..), runService)

data QueryResponseDTO = QueryResponseDTO
  { queryResponseId :: Text,
    message :: Text,
    date :: Text,
    links :: Int,
    urls :: [Text],
    text :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON QueryResponseDTO where
  toJSON = genericToJSON queryResponseJsonOptions

instance FromJSON QueryResponseDTO where
  parseJSON = genericParseJSON queryResponseJsonOptions

queryResponseJsonOptions :: Options
queryResponseJsonOptions = defaultOptions
  { fieldLabelModifier = \s ->
      case s of
        "queryResponseId" -> "id"
        other -> other
  }

data HomePageDTO = HomePageDTO
  { queryResponses :: [QueryResponseDTO]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SearchWebQuery = SearchWebQuery
  { searchWebQueryInputText :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON SearchWebQuery where
  parseJSON = withObject "SearchWebQuery" $ \o -> do
    inputText <- o .: "input_text"
    return $ SearchWebQuery inputText

type PageAPI =
  "home" :> Get '[PlainText] String
    :<|> "createNewsPage" :> Get '[PlainText] String
    :<|> "home-page" :> Get '[JSON] HomePageDTO

pageServer :: Pool SqlBackend -> AiNewsResponseService -> Server PageAPI
pageServer pool aiNewsResponseService =
  homeHandler
    :<|> createNewsPageHandler
    :<|> getHomePageHandler
  where
    homeHandler :: Handler String
    homeHandler = return "Welcome to the Home Page!"

    createNewsPageHandler :: Handler String
    createNewsPageHandler = return "This is the Create News Page!"

    getHomePageHandler :: Handler HomePageDTO
    getHomePageHandler = do
      liftIO $ putStrLn "get_home_page"
      buildDTO aiNewsResponseService

buildDTO :: AiNewsResponseService -> Handler HomePageDTO
buildDTO aiNewsResponseService = do
  aiNewsResponses <- runService aiNewsResponseService findAll

  let dtos = map modelToDTO aiNewsResponses
      sortedDtos = sortBy (comparing (Down . date)) dtos

  return $ HomePageDTO sortedDtos

modelToDTO :: Entity AiNewsResponse -> QueryResponseDTO
modelToDTO (Entity entityId aiNewsResponse) =
  QueryResponseDTO
    { queryResponseId = T.pack . show $ fromSqlKey entityId,
      message = aiNewsResponseInputMessage aiNewsResponse,
      date = aiNewsResponseDate aiNewsResponse,
      links = aiNewsResponseLinks aiNewsResponse,
      urls = parseUrls $ aiNewsResponseUrls aiNewsResponse,
      text = aiNewsResponseText aiNewsResponse
    }

parseUrls :: Text -> [Text]
parseUrls urlsText =
  if T.null urlsText
    then []
    else T.splitOn "," urlsText