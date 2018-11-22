module Truth where

import Control.Monad.IO.Class
import Config

generate :: MonadIO m => Config -> m ()
generate config = liftIO $ putStrLn $ show config
