module Helpers.Handler 
  ( unwrapServiceResult
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant (Handler, runHandler)

-- | Unwraps a Handler containing nested Either types, converting errors to IO failures.
-- This is particularly useful for service layer operations that return Handler (Either serviceError value).
-- 
-- The function handles two levels of errors:
-- 1. Servant-level errors (Left from runHandler)
-- 2. Service-level errors (Left from the inner Either)
--
-- Both types of errors are converted to IO failures with descriptive messages.
unwrapServiceResult :: (Show e, MonadIO m) => Handler (Either e a) -> m a
unwrapServiceResult handler = liftIO $ do
  result <- runHandler handler
  case result of
    Left servantErr -> fail $ "Servant error: " ++ show servantErr
    Right (Left serviceErr) -> fail $ "Service error: " ++ show serviceErr
    Right (Right value) -> return value