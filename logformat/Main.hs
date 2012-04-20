module Main where

import LogFormat

import Data.Map as M
import Text.Parsec

main = do putStrLn "Hello, world"

separator = do string "%"
               return Nothing

twoPathParser = combineMapBuilders [parserFor 'U', separator, parserFor 'U'] M.empty
