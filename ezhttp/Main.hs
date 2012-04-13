module Main where

import EZHTTP
import Control.Exception as E
import Text.JSON

main :: IO ()
main = do html <- get "http://harold.hotelling.net/"
          putStrLn $ "Web page has " ++ (show $ length html) ++ " characters."
          badRequest `E.catch` handleInvalidURL
          putStrLn "Recovered from the bad value safely."

instance Postable Int where
  contentType _ = "foo/bar"
  serialize = show

tryPost :: IO String
tryPost = let json = encJSDict [("a", 1 :: Int), ("b", 2)] in
            post url json
  where url = "http://harold.hotelling.net/"

badRequest :: IO ()
badRequest = do html2 <- get "abcdefghijklmnopqrstuvwxyz"
                putStrLn html2

handleInvalidURL :: InvalidURLException -> IO ()
handleInvalidURL e = do putStrLn message
  where message = "caught: " ++ show (e :: InvalidURLException)
