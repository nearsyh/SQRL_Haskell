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
    let (scheme, domain, path, query, challenge) = parseUrl url
    -- print scheme
    -- print domain
    -- print path
    -- print query
    -- print challenge

    let privateKey = privateKeyGen hashKey path
    let publicKey = makePubKey privateKey


    let (scheme, query) = getUrl url 1 ["enforce"] publicKey (pack "")
    let toSign = scheme ++ "//" ++ query
    let signature = cryptoSign publicKey privateKey toSign
    let verify = cryptoVerify publicKey signature toSign
    let newquery = if scheme == "sqrl:" then "https://" ++ query else "http://" ++ query
    -- print ("url = " ++ url)
    -- print ("challenge = " ++ challenge)
    -- print ("toSign = " ++ toSign)
    -- print ("publickey = " ++ (convert publicKey))
    -- print ("signature = " ++ (convert signature))
    -- print (verify)
    -- print ("newquery = " ++ newquery)
    issueRequest newquery (convert signature) 
