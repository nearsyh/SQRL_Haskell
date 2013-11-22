module SQRLUrl (
    parseUrl,
    getUrl,
    getBody
) where

import SQRLUtil
import Text.Regex
import Text.URI (queryToPairs)
import Data.String.Utils
import Network.URI
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

parseUrl :: String -> (String, String, String, String, String)
parseUrl url = let
    l = split "|" url
    newurl = replace "|" "/" url
  in
    case (parseURI) newurl of
      Just u -> if length l == 1 then
          (uriScheme u, getDomainFromAuth (uriAuthority u), uriPath u, uriQuery u, getChallenge (uriQuery u))
        else
          (uriScheme u, getDomainFromString (l !! 0), uriPath u, uriQuery u, getChallenge (uriQuery u))
      Nothing -> ("", "", "", "", "")

getUrl :: String -> Integer -> [String] -> ByteString -> ByteString -> (String, String, String)
getUrl url ver opt pubKey prioKey = q where
    (scheme, domain, path, query, challenge) = parseUrl url
    l = domain ++ path ++ query ++ (getVer ver) ++ (getOpt opt) ++ (getParam "&sqrlkey" pubKey) ++ (getParam "&sqrlold" prioKey)
    head = if scheme == "sqrl:" then "https:" else "http:"
    q = (scheme, head, l)

getBody :: ByteString -> ByteString -> String
getBody sig oldSig = (getParam "sqrlsig" sig) ++ (getParam "sqrlpri" oldSig)


----------------------
-- Helper Functions --
----------------------
getDomainFromAuth :: Maybe URIAuth -> String
getDomainFromAuth (Just auth) = r where
  (URIAuth u r p) = auth
getDomainFromAuth Nothing = ""

getDomainFromString :: String -> String
getDomainFromString url = last (split "//" url)

getChallenge :: String -> String
getChallenge query = nut where
  table = queryToPairs (tail query)
  nut = head [n | (k, n) <- table, k == "nut"]

getVer :: Integer -> String
getVer ver = "&sqrlver=" ++ (show ver)

getOpt :: [String] -> String
getOpt [] = ""
getOpt ls = "&sqrlopt=" ++ (foldl (\x -> \y -> x ++ "," ++ y) (head ls) (tail ls))

getParam :: String -> ByteString -> String
getParam name value
    | B.length value == 0 = ""
    | otherwise = name ++ "="  ++ (trim (convert value) '=')


----------------------
--       Test       --
----------------------
main = do
    let test = "sqrl://www.example.com/sqrl?hahahaha"
    print (parseUrl test)
