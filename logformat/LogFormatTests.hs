{-
    LogFormatTests.hs
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
module LogFormatTests where

import LogFormat

import Data.Map as M
import Test.HUnit
import Text.Parsec as P

runTests = runTestTT allTests

allTests = TestList [testU, testLiteral, testBadLit, testUAndLit,
                     testGetMethod, testPostMethod, testRemoteIP, testLocalIP]

data ParseResult a = Failure String
                   | SuccessForLiteral
                   | Success a
  deriving (Show, Eq, Ord)

-- Helper to build a bunch of parser test cases.
parserTest name message expected parser input =
    name ~: do assertEqual message expected actual
  where actual = let result = P.parse parser ("Unit Test: " ++ name) input in
                   case result of
                     Left parseError    -> Failure (show parseError)
                     Right Nothing      -> SuccessForLiteral
                     Right (Just value) -> Success value

literalParser lit = parserFor (Literal lit)

charRuleParser ch = parserFor (Keyword ch Nothing)

testU = parserTest "testU" "Should parse path" expected parser "/abc"
  where expected = Success ("path", "/abc")
        parser = do path <- charRuleParser 'U'
                    eof
                    return path

testLiteral = parserTest "testLiteral" "Should match literal" expected parser "hi"
  where expected = SuccessForLiteral :: ParseResult (String, String)
        parser = literalParser "hi"

testBadLit = parserTest "testBadLit" "Should fail to match literal" expected parser "def"
  where expected = Failure errMessage  :: ParseResult (String, String)
        errMessage = "\"Unit Test: testBadLit\" (line 1, column 1):\nunexpected \"d\"\nexpecting \"abc\""
        parser = literalParser "abc"

testUAndLit = parserTest "testUAndLit" "Should match a path and literal" expected parser "/path/to/somewhere?a=1"
  where expected = Success (M.fromList [("path", "/path/to/somewhere")])
        rawParser = combineMapBuilders [charRuleParser 'U', literalParser "?a=1"] M.empty
        parser = do result <- rawParser
                    return $ Just result

testGetMethod = parserTest "testGetMethod" "Should accept GET method" expected parser "methodGET"
  where expected = Success (M.fromList [("method", "GET")])
        rawParser = combineMapBuilders [literalParser "method", charRuleParser 'm'] M.empty
        parser = do result <- rawParser
                    return $ Just result


testPostMethod = parserTest "testPostMethod" "Should accept POST method" expected parser "methodPOST"
  where expected = Success (M.fromList [("method", "POST")])
        rawParser = combineMapBuilders [literalParser "method", charRuleParser 'm'] M.empty
        parser = do result <- rawParser
                    return $ Just result

testRemoteIP = parserTest "testRemoteIP" "Should handle remote IP address" expected parser "123.45.67.89"
  where expected = Success ("remoteIP", "123.45.67.89")
        parser = do path <- charRuleParser 'a'
                    eof
                    return path

testLocalIP = parserTest "testLocalIP" "Should handle local IP address" expected parser "123.45.67.89"
  where expected = Success ("localIP", "123.45.67.89")
        parser = do path <- charRuleParser 'A'
                    eof
                    return path

-- TODO : test these log formats

-- Common Log Format with Virtual Host
commonLogFormat = "%v %h %l %u %t \"%r\" %>s %b"

-- NCSA extended/combined log format
ncsaLogFormat = "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-agent}i\""