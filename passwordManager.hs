module PasswordManager (
    genPasswordKey,
    genVerification,
    verifyPassword
) where

import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, pack)
import Crypto.PBKDF.Core

genPasswordKey :: String -> ByteString
genPasswordKey password = pbkdf2 (sha256PBKDF password "" 1000 32)

genVerification :: ByteString -> ByteString
genVerification key = verification
    where
        hashedKey = hash key
        list = unpack key
        verification = pack [list !! i | i <- [0 .. (length list - 1)], 2 * i >= (length list)]

verifyPassword :: String -> ByteString -> Bool
verifyPassword input verification = hashedKey == verification
    where
        passwordKey = genPasswordKey input
        hashedKey = genVerification passwordKey

