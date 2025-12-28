module Password
  ( generatePassword,
    Profile (..),
    defaultProfile,
  )
where

import PBKDF2 (pbkdf2)

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

lowercase :: String
lowercase = "abcdefghijklmnopqrstuvwxyz"

uppercase :: String
uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

digits :: String
digits = "0123456789"

symbols :: String
symbols = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

defaultCharset :: String
defaultCharset = lowercase ++ uppercase ++ digits ++ symbols

consumeEntropy :: String -> Integer -> String -> Int -> (String, Integer)
consumeEntropy password entropy charset len =
  if length password >= len
    then (password, entropy)
    else
      let (entropy', remainder) = divMod entropy ((fromIntegral . length) charset)
          nextPass = password ++ [charset !! fromIntegral remainder]
       in consumeEntropy nextPass entropy' charset len

generatePassword :: String -> String -> IO String
generatePassword pass salt = do
  entropy <- pbkdf2 pass salt
  putStrLn defaultCharset
  let (generatedPassword, passwordEntropy) = consumeEntropy "" entropy defaultCharset 20
  return generatedPassword
