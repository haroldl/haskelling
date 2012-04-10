module EZHTTP (get) where

import Data.Char (intToDigit)
import Network.HTTP
import Network.Stream
import Network.URI

get :: String -> IO String
get url = case parseURI url of
               Nothing -> fail $ "Invalid URI: " ++ url
               Just uri -> getURI uri

getURI :: URI -> IO String
getURI uri = do response <- simpleHTTP (request uri)
                handleResponse response
  where handleResponse (Left  err) = fail $ "Failed request: " ++ (show err)
        handleResponse (Right rsp) = unpackResponse rsp
        unpackResponse rsp = case rspCode rsp of
                                  (2, 0, 0) -> return (rspBody rsp)
                                  _ -> fail $ "Bad response code: " ++ (httpError rsp)
        httpError r = showRspCode (rspCode r) ++ " " ++ rspReason r
        showRspCode (a,b,c) = map intToDigit [a,b,c]

request :: URI -> Request_String
request uri = Request { rqURI     = uri,
                        rqMethod  = GET,
                        rqHeaders = [],
                        rqBody    = "" }
