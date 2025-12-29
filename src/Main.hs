module Main where

import Password
import System.Console.GetOpt
import System.Environment (getArgs)

options :: [OptDescr (Profile -> Profile)]
options =
  [ Option
      ['C']
      ["counter"]
      (ReqArg (\val profile -> profile {passwordCounter = read val}) "COUNTER")
      "Password Counter",
    Option
      ['L']
      ["length"]
      (ReqArg (\val profile -> profile {passwordLength = read val}) "LENGTH")
      "Password character length"
  ]

parseArgs :: [String] -> IO (Profile, [String])
parseArgs argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultProfile o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: lesspass [OPTIONS] site login"

assignPositionalArgs :: Profile -> [String] -> Profile
assignPositionalArgs profile (userSite : userLogin : _) =
  profile {site = userSite, login = userLogin}
assignPositionalArgs _ _ = error "Missing positional arguments SITE and LOGIN"

main :: IO ()
main = do
  args <- getArgs
  (profile, positional) <- parseArgs args
  let finalProfile = assignPositionalArgs profile positional
  password <- generatePassword finalProfile "abcd"
  putStrLn password
