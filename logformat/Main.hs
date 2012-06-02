module Main where

import LogFormat

import Data.Map
import Text.JSON
import Text.Parsec

main = do (logFormat, parser) <- getLogFormatParser
          parseALogRecord logFormat parser

-- Ask for a LogFormat string, parse it to get a log record parser.
getLogFormatParser =
  do putStrLn "Enter your LogFormat string:"
     logFormat <- getLine
     case (logFormatParser logFormat) of
       Left parseErr ->
         fail $ invalidThingMessage "LogFormat string" logFormat parseErr
       Right parser ->
         return (logFormat, parser)

-- Ask for a log record, parse it, and print the resulting JSON object.
parseALogRecord logFormat parser =
  do putStrLn "Enter a log record to parse:"
     inputLine <- getLine
     case (parse parser ("LogFormat Tool [" ++ logFormat ++ "]") inputLine) of
       Left parseErr ->
         fail $ invalidThingMessage "log record" inputLine parseErr
       Right map ->
         putStrLn $ encode $ toJSObject $ toList map

invalidThingMessage thing input parseError =
  "Invalid " ++ thing ++ " \"" ++ input ++ "\": " ++ (show parseError)
