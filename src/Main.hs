module Main where

import Password (Profile (..), defaultProfile, generatePassword)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

options :: [OptDescr (Profile -> Profile)]
options =
  [ Option
      ['l']
      ["length"]
      (ReqArg (\len profile -> profile {passwordLength = read len}) "LENGTH")
      "Generated password length"
  ]

parseArgs :: [String] -> IO (Profile, [String])
parseArgs argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultProfile o, n)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitFailure
  where
    header = "Usage: hslp [-l] site login"

main :: IO ()
main = do
  (opts, positional) <- getArgs >>= parseArgs
  print opts
  print positional
