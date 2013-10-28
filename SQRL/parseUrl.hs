module ParseUrl (
    parseUrl,
    getUrl,
    getBody, 
    convert
) where

import Text.Regex
import Network.URL
import qualified Data.ByteString.Base64.URL as BU
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, pack)

parseUrl :: String -> (String, String, String)
parseUrl url = (domain, path, challenge) where
    -- TODO nut?
    l = splitRegex (mkRegex "\\?nut=") url
    challenge = tail (l !! 1)
    (domain, path) = parsePath (l !! 0)

getUrl :: String -> Integer -> [String] -> ByteString -> ByteString -> String
getUrl url ver opt pubKey prioKey = q where
    (header, path, challenge) = parseUrl url
    q = header ++ path ++ "?nut=" ++ challenge ++ (getVer ver) ++ (getOpt opt) ++ (getParam "&sqrlkey" pubKey) ++ (getParam "&sqrlold" prioKey)

getBody :: ByteString -> ByteString -> String
getBody sig oldSig = (getParam "sqrlsig" sig) ++ (getParam "sqrlpri" oldSig)


----------------------
-- Helper Functions --
----------------------
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
    | otherwise = name ++ "="  ++ (convert value)

convert :: ByteString -> String
convert bs = unpack (BU.encode bs)


----------------------
--       Test       --
----------------------
main = do
    let test = "sqrl://www.example.com/sqrl?hahahaha"
    print (parseUrl test)
