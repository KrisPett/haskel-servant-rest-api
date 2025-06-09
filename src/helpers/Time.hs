{-# LANGUAGE OverloadedStrings #-}

module Helpers.Time (parseUTC) where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

parseUTC :: String -> UTCTime
parseUTC str = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" str of
  Just t -> t
  Nothing -> error $ "Invalid UTC string: " ++ str
