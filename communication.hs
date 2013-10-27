module Communication (
    cryptoSign,
    privateKeyGen,
    makePubKey
) where

import Dust.Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Data.Digest.Pure.SHA

privateKeyGen :: ByteString -> String -> ByteString
privateKeyGen key msg = priKey where
    packMsg = pack msg
    digest = hmacSha256 (fromStrict key) (fromStrict packMsg)
    priKey = toStrict (bytestringDigest digest)

cryptoSign :: ByteString -> ByteString -> String -> ByteString
cryptoSign pubKey priKey url = signature where
    msg = pack url
    signature = ed25519_sign msg priKey pubKey

makePubKey :: ByteString -> ByteString
makePubKey priKey = ed25519_publickey priKey
