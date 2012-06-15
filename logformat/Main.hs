{-
    Main.hs
    Copyright (C) 2012 Harold Lee

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
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
