module LogFormatTests where

import LogFormat

import Data.Map as M
import Test.HUnit
import Text.Parsec as P

runTests = runTestTT allTests

allTests = TestList [testU, testLiteral, testBadLit, testUAndLit]

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

testU = parserTest "testU" "Should parse path" expected parser "/abc"
  where expected = Success ("path", "/abc")
        parser = do path <- parserFor 'U'
                    eof
                    return path

testLiteral = parserTest "testLiteral" "Should match literal" expected parser "hi"
  where expected = SuccessForLiteral :: ParseResult ()
        parser = literalParser "hi"

testBadLit = parserTest "testBadLit" "Should fail to match literal" expected parser "def"
  where expected = Failure errMessage  :: ParseResult ()
        errMessage = "\"Unit Test: testBadLit\" (line 1, column 1):\nunexpected \"d\"\nexpecting \"abc\""
        parser = literalParser "abc"

testUAndLit = parserTest "testUAndLit" "Should match a path and literal" expected parser "/path/to/somewhere?a=1"
  where expected = Success (M.fromList [("path", "/path/to/somewhere")])
        rawParser = combineMapBuilders [parserFor 'U', literalParser "?a=1"] M.empty
        parser = do result <- rawParser
                    return $ Just result
