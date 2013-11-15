module MasterkeyManager (
    loadMasterKey,
    saveMasterKey,
    xorByteString
) where

import System.Directory
import Dust.Ed25519
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack, pack)
import Crypto.PBKDF.Core
import Crypto.Random
import Data.Bits (xor, Bits)

loadMasterKey :: IO ByteString
loadMasterKey = do
    exist <- (doesFileExist masterkeyfile) 
    if exist then B.readFile masterkeyfile
    else generateMasterKey "" ""
    where masterkeyfile = ".masterkey"

saveMasterKey :: ByteString -> FilePath -> IO ()
saveMasterKey key path = do
    if path == "" then B.writeFile ".masterkey" key
    else B.writeFile path key
    

generateMasterKey :: String -> String -> IO ByteString
generateMasterKey old diff
    | length old == 0 =
        do
        g <- newGenIO :: IO SystemRandom
        let m = genBytes 1 (g :: SystemRandom)
        let Right (salt, g) = m
        let s = unpack salt
        return (pbkdf2 (sha256PBKDF "" s 1000 32))
    | otherwise = do
        let old2 = pack old
        let diff2 = pack diff
        return (xorByteString old2 diff2)

xorByteString :: ByteString -> ByteString -> ByteString
xorByteString a b = B.pack lc
    where
        la = B.unpack a
        lb = B.unpack b
        lc = [xor x y | (x, y) <- zip la lb]
