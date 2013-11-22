module SQRLUtil (
    xorByteString,
    convert,
    trim
) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Bits (xor, Bits)
import qualified Data.ByteString.Base64.URL as BU
import Data.ByteString.Char8 (unpack, pack)

xorByteString :: ByteString -> ByteString -> ByteString
xorByteString a b = B.pack lc
    where
        la = B.unpack a
        lb = B.unpack b
        lc = [xor x y | (x, y) <- zip la lb]

convert :: ByteString -> String
convert bs = unpack (BU.encode bs)

trim :: String -> Char -> String
trim s a = ns where
  ns = [i | i <- s, i /= a]
