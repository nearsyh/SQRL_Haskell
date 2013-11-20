module Main where

import System.Directory
import MasterkeyManager
import Communication
import ParseUrl
import PasswordManager
import MyNetwork
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack, pack)

main = do
    masterKey <- loadMasterKey
    saveMasterKey masterKey ""

    let password = "hahahaha"
    let passwordKey = genPasswordKey password

    let hashKey = xorByteString passwordKey masterKey

    url <- getLine
    let (domain, path, challenge) = parseUrl url

    let privateKey = privateKeyGen hashKey path
    let publicKey = makePubKey privateKey


    let query = getUrl url 1 ["enforce"] publicKey (pack "")
    let toSign = "sqrl://" ++ query
    let signature = cryptoSign publicKey privateKey toSign
    let verify = cryptoVerify publicKey signature toSign
    let newquery = "https://" ++ query
    -- let body = getBody signature (pack "")
    -- print ("url = " ++ url)
    -- print ("challenge = " ++ challenge)
    -- print ("toSign = " ++ toSign)
    -- print ("publickey = " ++ (convert publicKey))
    -- print ("signature = " ++ (convert signature))
    -- print ("verify = " ++ verify)
    -- print ("newquery = " ++ newquery)
    -- print ("body = " ++ body)
    issueRequest newquery (convert signature) 
