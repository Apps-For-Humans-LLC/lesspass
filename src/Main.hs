module Main where

import PBKDF2 (withKdfCtx)

-- | Reference: def _calc_entropy(password_profile, master_password):
-- | salt = (
-- | password_profile["site"]
-- |     + password_profile["login"]
-- |     + hex(password_profile["counter"])[2:]
-- |     )
-- |     hex_entropy = hashlib.pbkdf2_hmac(
-- |     "sha256", master_password.encode("utf-8"), salt.encode("utf-8"), 100000, 32
-- |     ).hex()
-- |     return int(hex_entropy, 16)
main :: IO ()
main = withKdfCtx "pbkdf2" print
