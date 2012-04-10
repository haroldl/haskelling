module Main where

import EZHTTP

main = do html <- get "http://harold.hotelling.net/"
          putStrLn html
          html2 <- get "ntahoenuthantoehutnah"
          putStrLn html2
