module MyNetwork (
    login
) where

import SQRLCrypto
import qualified SQRLUrl as SURL
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network (withSocketsDo)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Default

login :: ByteString -> String -> String -> IO (Response LB.ByteString)
login masterKey password url = do
    let (scheme, domain, path, query, challenge) = SURL.parseUrl url
    print domain
    let (privateKey, publicKey) = genKeyPair masterKey password domain
    let (scheme, head, query) = SURL.getUrl url 1 [] publicKey (pack "")
    let (sig, pub) = genSigAndPub (privateKey, publicKey) (scheme ++ "//" ++ query)
    print sig
    issueRequest (head ++ "//" ++ query) sig

getRequest :: (Monad m) => String -> String -> IO (Request m)
getRequest url body = do
    initReq <- parseUrl url
    let req' = initReq { secure = True, method = methodPost }
    let req = (flip urlEncodedBody) req' $ [((pack "sqrlsig"), (pack body))]
    print req
    return req

issueRequest :: String -> String -> IO (Response LB.ByteString)
issueRequest url body = withSocketsDo $ do
    r <- getRequest url body
    res <- withManager $ httpLbs r
    return res
