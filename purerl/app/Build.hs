module Build (inferForeignModule') where

import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import Prelude

inferForeignModule' :: MonadIO m => FilePath -> m (Maybe FilePath)
inferForeignModule' path = do
  let jsFile = replaceExtension path "erl"
  exists <- liftIO $ doesFileExist jsFile
  if exists
    then return (Just jsFile)
    else return Nothing
