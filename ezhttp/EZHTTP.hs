{-# LANGUAGE DeriveDataTypeable #-}

module EZHTTP (get, post, queryString, InvalidURLException, HttpException) where

import Control.Exception (throwIO)
import Control.Exception.Base
import Data.Char (intToDigit)
import Data.Typeable
import Network.HTTP
import Network.Stream
import Network.URI

get :: String -> IO String
get url = executeHttp url request

post :: String -> [(String, String)] -> IO String
post url params = executeHttp url (\uri -> postReq uri params)

executeHttp :: String -> (URI -> Request String) -> IO String
executeHttp url request = do uri <- parseURI' url
                             response <- makeRequest (request uri)
                             checkStatus response
                             return (rspBody response)

request :: URI -> Request String
request uri = Request { rqURI     = uri,
                        rqMethod  = GET,
                        rqHeaders = [],
                        rqBody    = "" }

postReq :: URI -> [(String, String)] -> Request String
postReq uri params = Request { rqURI     = uri,
                               rqMethod  = POST,
                               rqHeaders = [contentTypeHeader, contentLengthHeader],
                               rqBody    = qs }
  where contentTypeHeader = Header HdrContentType "application/x-www-form-urlencoded"
        contentLengthHeader = Header HdrContentLength (show $ length $ qs)
        qs = queryString params

queryString :: [(String, String)] -> String
queryString [] = ""
queryString ps = foldl1 (\p1 p2 -> p1 ++ "&" ++ p2) (map encode ps)
 where encode (name, value) = (urlEncode name) ++ "=" ++ (urlEncode $ value)

-- Send the HTTP request and return the response.
-- Throw an HttpException if the request fails.
makeRequest :: HStream a => Request a -> IO (Response a)
makeRequest request = do result <- simpleHTTP request
                         checkResult result
  where checkResult (Left  err) = failedRequest err
        checkResult (Right rsp) = return rsp
        failedRequest err = throwIO $ HttpException $ "Failed request: " ++ (show err)

-- Make sure the respose was successful, throw an HttpException otherwise.
checkStatus :: Response a -> IO ()
checkStatus rsp = case rspCode rsp of
                       (2, 0, 0) -> return ()
                       _ -> badStatusCode rsp
  where badStatusCode rsp = throwIO $ HttpException $ "Bad status code: " ++ (httpError rsp)
        httpError rsp = showRspCode (rspCode rsp) ++ " " ++ rspReason rsp
        showRspCode (a,b,c) = map intToDigit [a,b,c]

-- Take parseURI and move the result from the Maybe monad to the IO monad,
-- throwing an exception if the result was Nothing.
parseURI' :: String -> IO URI
parseURI' url = maybe err return (parseURI url)
  where err = throwIO $ InvalidURLException $ "Invalid URI: " ++ url

-- Exceptions

data InvalidURLException = InvalidURLException { url :: String }
  deriving (Show, Eq, Typeable)

instance Exception InvalidURLException

data HttpException = HttpException { message :: String }
  deriving (Show, Eq, Typeable)

instance Exception HttpException
