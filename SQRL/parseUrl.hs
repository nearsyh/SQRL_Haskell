module ParseUrl (
    parseUrl,
    getUrl,
    getBody, 
    convert
) where

import Text.Regex
import Text.URI (queryToPairs)
import Data.String.Utils
import Network.URL
import Network.URI
import qualified Data.ByteString.Base64.URL as BU
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, pack)

--parseUrl2 :: String -> (String, String, String)
--parseUrl2 url = (scheme, domain, path, challenge) where
--    -- TODO nut?
--    l = splitRegex (mkRegex "\\?nut=") url
--    print l
--    challenge = (l !! 1)
--    (scheme, domain, path) = parsePath (l !! 0)

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

getUrl :: String -> Integer -> [String] -> ByteString -> ByteString -> (String, String)
getUrl url ver opt pubKey prioKey = q where
    (scheme, domain, path, query, challenge) = parseUrl url
    q = (scheme, domain ++ path ++ query ++ (getVer ver) ++ (getOpt opt) ++ (getParam "&sqrlkey" pubKey) ++ (getParam "&sqrlold" prioKey))

getBody :: ByteString -> ByteString -> String
getBody sig oldSig = (getParam "sqrlsig" sig) ++ (getParam "sqrlpri" oldSig)


----------------------
-- Helper Functions --
----------------------
getDomainFromAuth :: Maybe URIAuth -> String
getDomainFromAuth (Just auth) = u ++ r ++ p where
  (URIAuth u r p) = auth
getDomainFromAuth Nothing = ""

getDomainFromString :: String -> String
getDomainFromString url = last (split "//" url)

getChallenge :: String -> String
getChallenge query = nut where
  table = queryToPairs (tail query)
  nut = head [n | (k, n) <- table, k == "nut"]

parsePath :: String -> (String, String)
parsePath url = 
    case importURL url of
        Just (URL u_type u_path _) -> ((parseType u_type), "/" ++ u_path)
        Nothing -> ("", "")

parseType :: URLType -> String
parseType u_type = 
    case u_type of
        Absolute (Host p h port) ->
            case port of
                Just po -> h ++ ":" ++ (show po)
                Nothing -> h
        HostRelative -> ""
        PathRelative -> ""

getVer :: Integer -> String
getVer ver = "&sqrlver=" ++ (show ver)

getOpt :: [String] -> String
getOpt [] = ""
getOpt ls = "&sqrlopt=" ++ (foldl (\x -> \y -> x ++ "," ++ y) (head ls) (tail ls))

getParam :: String -> ByteString -> String
getParam name value
    | B.length value == 0 = ""
    | otherwise = name ++ "="  ++ (trim (convert value) '=')

convert :: ByteString -> String
convert bs = unpack (BU.encode bs)

trim :: String -> Char -> String
trim s a = ns where
  ns = [i | i <- s, i /= a]


----------------------
--       Test       --
----------------------
main = do
    let test = "sqrl://www.example.com/sqrl?hahahaha"
    print (parseUrl test)
