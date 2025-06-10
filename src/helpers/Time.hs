{-# LANGUAGE OverloadedStrings #-}

module Helpers.Time 
  (
    parseUTC
    , parseUrls
  ) where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Text (Text)
import qualified Data.Text as T

parseUTC :: String -> UTCTime
parseUTC str = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" str of
  Just t -> t
  Nothing -> error $ "Invalid UTC string: " ++ str

parseUrls :: Text -> [Text]
parseUrls urlsText =
  if T.null urlsText
    then []
    else T.splitOn "," urlsText