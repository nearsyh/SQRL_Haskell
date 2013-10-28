module Main where

import MasterkeyManager
import Communication
import ParseUrl
import PasswordManager
import MyNetwork
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack, pack)

main = do
    masterKey <- generateMasterKey "" ""

    let password = "hahahaha"
    let passwordKey = genPasswordKey password

    let hashKey = xorByteString passwordKey masterKey

    let url = "sqrl://www.example.com/sqrl?nut=hahahahahaha"
    let (domain, path, challenge) = parseUrl url

    let privateKey = privateKeyGen hashKey path
    let publicKey = makePubKey privateKey
    let toSign = domain ++ path ++ "?" ++ challenge
    let signature = cryptoSign publicKey privateKey toSign
    print ("publickey = " ++ (convert publicKey))
    print ("signature = " ++ (convert signature))

    let query = getUrl url 1 ["enforce"] publicKey (pack "")
    let body = getBody signature (pack "")
    print query
    print body

    let verify = cryptoVerify publicKey signature toSign
    print verify

    issueRequest query body
