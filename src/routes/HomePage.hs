{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.HomePage
  ( HomePageAPI,
    homePageServer,
  )
where

import Configs.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (..),
    Options (fieldLabelModifier),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    withObject,
    (.:),
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
import Helpers.Time (parseUrls)
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
queryResponseJsonOptions =
  defaultOptions
    { fieldLabelModifier = \s ->
        case s of
          "queryResponseId" -> "id"
          other -> other
    }

data HomePageDTO = HomePageDTO
  { queryResponses :: [QueryResponseDTO]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type HomePageAPI = "home_page" :> Get '[JSON] HomePageDTO

homePageServer :: Pool SqlBackend -> AiNewsResponseService -> Server HomePageAPI
homePageServer pool aiNewsResponseService = getHomePageHandler
  where
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
