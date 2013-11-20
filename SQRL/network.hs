module MyNetwork (
    getRequest,
    issueRequest,
    getResponseStatus,
    getResponseVersion,
    getResponseHeaders,
    getResponseBody,
    getResponseCookieJar
) where

import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network (withSocketsDo)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Default

getRequest :: (Monad m) => String -> String -> IO (Request m)
getRequest url body = do
    initReq <- parseUrl url
    --let req = initReq { method = methodPost,
    --            secure = True,
    --            requestBody = RequestBodyBS (pack body)}
    let req' = initReq { secure = True, method = methodPost }
    let req = (flip urlEncodedBody) req' $ [((pack "sqrlsig"), (pack body))]
    return req

issueRequest :: String -> String -> IO (Response LB.ByteString)
issueRequest url body = withSocketsDo $ do
    r <- getRequest url body
    res <- withManager $ httpLbs r
    return res

getResponseStatus = responseStatus
getResponseVersion = responseVersion
getResponseHeaders = responseHeaders
getResponseBody = responseBody
getResponseCookieJar = responseCookieJar
