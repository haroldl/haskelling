{-# LANGUAGE DeriveDataTypeable #-}

-- | A simple HTTP module for easy scripting or interactive use.
module EZHTTP (get, post, queryString, InvalidURLException, HttpException) where

import Control.Exception (throwIO)
import Control.Exception.Base
import Data.Char (intToDigit)
import Data.Typeable
import Network.HTTP
import Network.Stream
import Network.URI

-- | Make an HTTP GET request for the URL given and return the body of the response as a String.
--
-- Simple example to print the HTML of a page:
--
-- > do html <- get "http://harold.hotelling.net/"
-- >    putStrLn html
--
-- Or catch the exceptions with 'Control.Exception.catch':
--
-- > import Control.Exception as E
-- >
-- > do html <- get "http://harold.hotelling.net/" `E.catch` errorHandler
-- >    putStrLn html
--
-- Might throw 'InvalidURLException' or 'HttpException'.
get :: String -> IO String
get url = executeHttp url request

-- | Make an HTTP GET request for the URL given sending the list of parameters as if submitting an
--   HTML form and return the body of the response as a String.
--
--   Might throw 'InvalidURLException' or 'HttpException'.
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

{-|
   A helper function to take a set of parameters and urlencode them so that you can add them to the
   query string of a URL or the body of a urlencoded POST request.
 -}
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

{-|
   This is thrown when a String passed in as a URL fails to parse.
 -}
data InvalidURLException = InvalidURLException { url :: String }
  deriving (Show, Eq, Typeable)

instance Exception InvalidURLException

{-|
   This is thrown when an HTTP request fails, either because of some IO error or because the status
   code was something other than 200 OK.
 -}
data HttpException = HttpException { message :: String }
  deriving (Show, Eq, Typeable)

instance Exception HttpException
