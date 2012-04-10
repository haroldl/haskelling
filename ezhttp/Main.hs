module Main where

import EZHTTP
import Control.Exception as E

main :: IO ()
main = do html <- get "http://harold.hotelling.net/"
          putStrLn $ "Web page has " ++ (show $ length html) ++ " characters."
          badRequest `E.catch` handleInvalidURL
          putStrLn "Recovered from the bad value safely."

badRequest :: IO ()
badRequest = do html2 <- get "abcdefghijklmnopqrstuvwxyz"
                putStrLn html2

handleInvalidURL :: InvalidURLException -> IO ()
handleInvalidURL e = do putStrLn message
  where message = "caught: " ++ show (e :: InvalidURLException)
