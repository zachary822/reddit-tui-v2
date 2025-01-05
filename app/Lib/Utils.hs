module Lib.Utils where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import System.Info
import System.Process

openInBrowser :: Text -> IO ()
openInBrowser url = do
  guard (os == "darwin")
  void $ createProcess (proc "open" [T.unpack url])
