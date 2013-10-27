module Main where

import MasterkeyManager
import Communication
import ParseUrl
import PasswordManager
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack, pack)

main = do
    masterKey <- generateMasterKey "" ""

    let password = "hahahaha"
    let passwordKey = genPasswordKey password

    let hashKey = xorByteString passwordKey masterKey

    let url = "www.example.com/sqrl?hahahahahaha"
    let (path, challenge) = parseUrl url

    let privateKey = privateKeyGen hashKey path
    let publicKey = makePubKey privateKey
    let signature = cryptoSign publicKey privateKey challenge
    print ("publickey = " ++ (unpack publicKey))
    print ("signature = " ++ (unpack signature))

