module Helpers.Handler
  ( unwrapServiceResult,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant (Handler, runHandler)

unwrapServiceResult :: (MonadIO m) => Handler a -> m a
unwrapServiceResult handlerAction = liftIO $ do
  result <- runHandler handlerAction
  case result of
    Left err -> fail $ "Handler error: " ++ show err
    Right val -> return val