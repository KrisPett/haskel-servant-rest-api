{-# LANGUAGE OverloadedStrings #-}

module Middlewares.ErrorHandler
  ( errorHandler,
    AppError (..),
    throwAppError,
  )
where

import Control.Exception (Exception, SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Servant

-- Custom error types
data AppError
  = DatabaseError String
  | ValidationError String
  | NotFoundError String
  | InternalError String
  deriving (Show, Eq)

instance Exception AppError

instance ToJSON AppError where
  toJSON (DatabaseError msg) =
    object
      [ "error" .= ("Database Error" :: String),
        "message" .= msg,
        "code" .= (500 :: Int)
      ]
  toJSON (ValidationError msg) =
    object
      [ "error" .= ("Validation Error" :: String),
        "message" .= msg,
        "code" .= (400 :: Int)
      ]
  toJSON (NotFoundError msg) =
    object
      [ "error" .= ("Not Found" :: String),
        "message" .= msg,
        "code" .= (404 :: Int)
      ]
  toJSON (InternalError msg) =
    object
      [ "error" .= ("Internal Error" :: String),
        "message" .= msg,
        "code" .= (500 :: Int)
      ]

-- Helper to throw custom errors
throwAppError :: AppError -> Handler a
throwAppError (DatabaseError msg) = throwError err500 {errBody = encode (DatabaseError msg)}
throwAppError (ValidationError msg) = throwError err400 {errBody = encode (ValidationError msg)}
throwAppError (NotFoundError msg) = throwError err404 {errBody = encode (NotFoundError msg)}
throwAppError (InternalError msg) = throwError err500 {errBody = encode (InternalError msg)}

-- Generic error handler for IO operations
errorHandler :: IO a -> Handler a
errorHandler action = liftIO $ action `catch` handleException
  where
    handleException :: SomeException -> IO a
    handleException e = do
      putStrLn $ "Error occurred: " ++ show e
      error $ "Internal server error: " ++ show e