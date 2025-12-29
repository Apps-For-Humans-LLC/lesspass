module Main where

import System.Console.GetOpt
import System.Environment (getArgs)

data Profile = Profile
  { useLowercase :: Bool,
    useUppercase :: Bool,
    useDigits :: Bool,
    useSymbols :: Bool,
    passwordLength :: Int,
    passwordCounter :: Int,
    site :: String,
    login :: String,
    exclude :: String
  }
  deriving (Show)

defaultProfile :: Profile
defaultProfile =
  Profile
    { useLowercase = True,
      useUppercase = True,
      useSymbols = True,
      useDigits = True,
      passwordLength = 16,
      passwordCounter = 1,
      site = "",
      login = "",
      exclude = ""
    }

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
  print finalProfile
