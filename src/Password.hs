module Password
  ( generatePassword,
  )
where

import PBKDF2 (pbkdf2)

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
  let (generatedPassword, _) = consumeEntropy "" entropy defaultCharset 20
  return generatedPassword
