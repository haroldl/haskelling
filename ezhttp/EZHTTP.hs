{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A simple HTTP module for easy scripting or interactive use.
module EZHTTP (get, post, queryString,
               Postable, contentType, serialize,
               InvalidURLException, HttpException) where

import Control.Exception (throwIO)
import Control.Exception.Base
import Data.Char (intToDigit)
import Data.Typeable
import Network.HTTP
import Network.Stream
import Network.URI
import Text.JSON
import Text.XML.Light

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
get url = executeHttp url request []

-- | Make an HTTP GET request for the URL given sending the list of parameters as if submitting an
--   HTML form and return the body of the response as a String.
--
--   You can post anything that is 'Postable'. Several options are pre-defined and the correct
--   Content-Type is used automatically.
--
--     * Strings (sent as text/plain)
--
--     * Form parameters
--
--     * JSON (cabal install json, then import Text.JSON)
--
--     * XML (cabal install xml, then import Text.XML.Light)
--
--    For example:
--
--    > -- Post url-encoded form inputs: a=1&b=2
--    > post url [("a", "1"), ("b", "2")]
--    >
--    > -- Post JSON: {"a": 1, "b": 2}
--    > let json = encJSDict [("a", 1 :: Int), ("b", 2)] in
--    >   post url json
--    >
--    > -- Post XML: <doc><a>1</a><b>2</b></doc>
--    > case parseXMLDoc "<doc><a>1</a><b>2</b></doc>" of
--    >   Just xml -> post url xml
--    >   Nothing -> ...
--
--   You can add support for POSTing any other data type by adding an instance
--   declaration in your code like so:
--
--    > instance Postable MyType where
--    >   contentType _ = "some/type"
--    >   serialize value = ...
--
--   Might throw 'InvalidURLException' or 'HttpException'.
post :: Postable a => String -> a -> IO String
post url params = executeHttp url (\uri -> postReq uri params) []

-- | Class of types that can be sent via HTTP POST.
class Postable a where

  -- | Give the Content-Type header value for this data type.
  contentType :: a -> String

  -- | Convert the value to a String to send in the body of a POST request.
  serialize :: a -> String

instance Postable [Char] where
  contentType _ = "text/plain"
  serialize = id

instance Postable [(String, String)] where
  contentType _ = "application/x-www-form-urlencoded"
  serialize = queryString

instance Postable JSValue where
  contentType _ = "application/json"
  serialize = encode

instance Postable Element where
  contentType _ = "application/xml"
  serialize = showElement

executeHttp :: String -> (URI -> Request String) -> [URI] -> IO String
executeHttp url request visited = do uri <- parseURI' url
                                     response <- makeRequest (request uri)
                                     checkStatus response
                                     return (rspBody response)

request :: URI -> Request String
request uri = Request { rqURI     = uri,
                        rqMethod  = GET,
                        rqHeaders = [],
                        rqBody    = "" }

postReq :: Postable a => URI -> a -> Request String
postReq uri params = Request { rqURI     = uri,
                               rqMethod  = POST,
                               rqHeaders = [contentTypeHeader, contentLengthHeader],
                               rqBody    = body }
  where contentTypeHeader = Header HdrContentType (contentType params)
        contentLengthHeader = Header HdrContentLength (show $ length $ body)
        body = serialize params

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
