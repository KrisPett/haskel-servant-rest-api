{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Models where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (Migration, PersistFieldSql (..), SqlType (SqlOther))
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
AiGeneratedArticle
    createdAt UTCTime
    updatedAt UTCTime
    title Text
    description Text Maybe
    content Text
    deriving Show Eq Generic

AiNewsCategorized
    createdAt UTCTime
    updatedAt UTCTime
    title Text
    description Text
    content Text
    deriving Show Eq Generic

AiNewsResponse
    createdAt UTCTime
    updatedAt UTCTime
    inputMessage Text
    date Text
    links Int
    urls Text
    text Text
    deriving Show Eq Generic
|]
