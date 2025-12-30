module Password.Read (readPassword) where

import System.IO (hSetEcho, stdin)

readPassword :: IO String
readPassword = do
  hSetEcho stdin False
  pass <- getLine
  hSetEcho stdin True
  return pass
