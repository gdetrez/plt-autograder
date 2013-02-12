module Utils where

import Control.Monad.IO.Class (MonadIO(..))
import System.Console.ANSI

debug :: (MonadIO m) => String -> m ()
debug s = liftIO $ do
  setSGR [SetColor Foreground Dull Magenta]
  putStrLn s
  setSGR [Reset]

