module Main where

import Password
import Password.Read (readPassword)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

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
      "Password character length",
    Option
      ['l']
      ["lowercase"]
      (NoArg (\profile -> profile {useLowercase = True}))
      "Add lowercase characters in password",
    Option
      ['u']
      ["uppercase"]
      (NoArg (\profile -> profile {useUppercase = True}))
      "Add uppercase characters in password",
    Option
      ['d']
      ["digits"]
      (NoArg (\profile -> profile {useDigits = True}))
      "Add digits in password",
    Option
      ['s']
      ["symbols"]
      (NoArg (\profile -> profile {useSymbols = True}))
      "Add symbols in password"
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

assignCharsets :: Profile -> Profile
assignCharsets profile =
  let lowercase = useLowercase profile
      uppercase = useUppercase profile
      digits = useDigits profile
      symbols = useSymbols profile
   in if not lowercase && not uppercase && not digits && not symbols
        then
          profile
            { useLowercase = True,
              useUppercase = True,
              useDigits = True,
              useSymbols = True
            }
        else profile

main :: IO ()
main = do
  args <- getArgs
  (profile, positional) <- parseArgs args
  let siteLoginProfile = assignPositionalArgs profile positional
      finalProfile = assignCharsets siteLoginProfile
  finalProfile `seq` putStr "Master Password: "
  hFlush stdout
  masterPassword <- readPassword
  password <- generatePassword finalProfile masterPassword
  putStrLn ("\n" ++ password)
