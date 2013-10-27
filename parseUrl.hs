module ParseUrl (
    parseUrl
) where

import Text.Regex
import Network.URL

parseUrl :: String -> (String, String)
parseUrl url = (path, challenge) where
    l = splitRegex (mkRegex "/sqrl") url
    challenge = tail (l !! 1)
    path = parsePath (l !! 0)

parsePath :: String -> String
parsePath url = 
    case importURL url of
        Just (URL u_type u_path _) -> (parseType u_type) ++ u_path
        Nothing -> ""

parseType :: URLType -> String
parseType u_type = 
    case u_type of
        Absolute (Host p h port) ->
            case port of
                Just po -> h ++ ":" ++ (show po)
                Nothing -> h
        HostRelative -> ""
        PathRelative -> ""

main = do
    let test = "https://www.example.com/sqrl?hahahaha"
    print (parseUrl test)
