module Password
  ( generatePassword,
    Profile (..),
    defaultProfile,
  )
where

import Data.List (foldl')
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
    { useLowercase = False,
      useUppercase = False,
      useSymbols = False,
      useDigits = False,
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

-- defaultCharset :: String
-- defaultCharset = lowercase ++ uppercase ++ digits ++ symbols

consumeEntropy :: String -> Integer -> String -> Int -> (String, Integer)
consumeEntropy password entropy charset len =
  if length password >= len
    then (password, entropy)
    else
      let (entropy', remainder) = divMod entropy ((fromIntegral . length) charset)
          nextPass = password ++ [charset !! fromIntegral remainder]
       in consumeEntropy nextPass entropy' charset len

calcEntropy :: Profile -> String -> IO Integer
calcEntropy profile password =
  pbkdf2 password salt
  where
    salt = site profile ++ login profile ++ show (passwordCounter profile)

getCharsets :: Profile -> [String]
getCharsets profile =
  let ruleChecks = [(useLowercase, lowercase), (useUppercase, uppercase), (useDigits, digits), (useSymbols, symbols)]
      charsets = map (\(fn, chars) -> if fn profile then chars else "") ruleChecks
   in filter (not . null) charsets

getCharsForRules :: Integer -> [String] -> (String, Integer)
getCharsForRules entropy =
  foldl'
    ( \(out, nextEntropy) charset ->
        let (char, entropy') = consumeEntropy "" nextEntropy charset 1 in (out ++ char, entropy')
    )
    ("", entropy)

insertRandomChar :: (String, Integer) -> Char -> (String, Integer)
insertRandomChar (pass, entropy) letter =
  let (quotient, remainder) = divMod entropy ((fromIntegral . length) pass)
   in (take (fromIntegral remainder) pass ++ [letter] ++ drop (fromIntegral remainder) pass, quotient)

insertChars :: String -> Integer -> String -> String
insertChars pass entropy chars =
  result
  where
    (result, _) = foldl' insertRandomChar (pass, entropy) chars

renderPassword :: Integer -> Profile -> String
renderPassword entropy profile =
  -- TODO Get charset from all the use* things in Profile
  -- TODO Not sure how to port the "rules" part yet.
  let charsets = getCharsets profile
      (pass, passEntropy) = consumeEntropy "" entropy (mconcat charsets) (passwordLength profile - length charsets)
      (chars, charEntropy) = getCharsForRules passEntropy charsets
   in insertChars pass charEntropy chars

generatePassword :: Profile -> String -> IO String
generatePassword profile password = do
  entropy <- calcEntropy profile password
  return (renderPassword entropy profile)
