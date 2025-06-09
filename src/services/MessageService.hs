{-# LANGUAGE OverloadedStrings #-}

module Services.MessageService
  ( createMessage,
    getAllMessages,
  )
where

import Configs.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Entity.Models (EntityField (..), Message (..), MessageId)
import Middlewares.ErrorHandler
import Servant

createMessage :: Pool SqlBackend -> Text -> Handler MessageId
createMessage pool content = do
  result <- liftIO $ runDB pool $ insert $ Message content
  return result

createMessageSafe :: Pool SqlBackend -> Text -> Handler MessageId
createMessageSafe pool content = do
  result <- errorHandler $ runDB pool $ insert $ Message content
  return result

getAllMessages :: Pool SqlBackend -> Handler [Entity Message]
getAllMessages pool = do
  result <- errorHandler $ runDB pool $ selectList [] []
  return result

getAllMessagesContent :: Pool SqlBackend -> Handler [Text]
getAllMessagesContent pool = do
  entities <- errorHandler $ runDB pool $ selectList [] []
  return $ map (messageContent . entityVal) entities

getMessageById :: Pool SqlBackend -> MessageId -> Handler (Maybe (Entity Message))
getMessageById pool msgId = do
  maybeMsg <- liftIO $ runDB pool $ get msgId
  case maybeMsg of
    Just msg -> return $ Just (Entity msgId msg)
    Nothing -> return Nothing

updateMessage :: Pool SqlBackend -> MessageId -> Text -> Handler ()
updateMessage pool msgId newContent = do
  result <- liftIO $ runDB pool $ update msgId [MessageContent =. newContent]
  return result

deleteMessage :: Pool SqlBackend -> MessageId -> Handler ()
deleteMessage pool msgId = do
  result <- errorHandler $ runDB pool $ delete msgId
  return result

countMessages :: Pool SqlBackend -> Handler Int
countMessages pool = do
  count <- liftIO $ runDB pool $ count ([] :: [Filter Message])
  return count

searchMessages :: Pool SqlBackend -> Text -> Handler [Entity Message]
searchMessages pool searchTerm = do
  result <- errorHandler $ runDB pool $ selectList [] []
  return $ filter (\entity -> searchTerm `isInfixOf` messageContent (entityVal entity)) result
  where
    isInfixOf needle haystack = needle `T.isInfixOf` haystack

getMessagesPaginated :: Pool SqlBackend -> Int -> Int -> Handler [Entity Message]
getMessagesPaginated pool offset limit = do
  result <- errorHandler $ runDB pool $ selectList [] [OffsetBy offset, LimitTo limit]
  return result
