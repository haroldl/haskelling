{-# LANGUAGE DeriveDataTypeable #-}

module EZHTTP (get, InvalidURLException, HttpException) where

import Control.Exception (throwIO)
import Control.Exception.Base
import Data.Char (intToDigit)
import Data.Typeable
import Network.HTTP
import Network.Stream
import Network.URI

get :: String -> IO String
get url = case parseURI url of
               Nothing -> throwIO $ InvalidURLException $ "Invalid URI: " ++ url
               Just uri -> getURI uri

getURI :: URI -> IO String
getURI uri = do response <- simpleHTTP (request uri)
                handleResponse response
  where handleResponse (Left  err) = throwIO $ HttpException $ "Failed request: " ++ (show err)
        handleResponse (Right rsp) = unpackResponse rsp
        unpackResponse rsp = case rspCode rsp of
                                  (2, 0, 0) -> return (rspBody rsp)
                                  _ -> throwIO $ HttpException $ "Bad response code: " ++ (httpError rsp)
        httpError r = showRspCode (rspCode r) ++ " " ++ rspReason r
        showRspCode (a,b,c) = map intToDigit [a,b,c]

request :: URI -> Request_String
request uri = Request { rqURI     = uri,
                        rqMethod  = GET,
                        rqHeaders = [],
                        rqBody    = "" }

data InvalidURLException = InvalidURLException { url :: String }
  deriving (Show, Eq, Typeable)

instance Exception InvalidURLException

data HttpException = HttpException { message :: String }
  deriving (Show, Eq, Typeable)

instance Exception HttpException
