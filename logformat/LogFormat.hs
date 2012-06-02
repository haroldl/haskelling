{-# LANGUAGE NoMonomorphismRestriction #-}
{- | LogFormat is a Haskell module that makes it trivial to parse access
     log records.
 -}
module LogFormat where

import Data.Map
import Text.Parsec

-- | Parser a is a Parsec parser for Strings that parses an 'a'.
type Parser a = Parsec String () a

-- | A LogFormat string is made up of literal strings (which must match
--   exactly) and % directives that match a certain pattern and can have
--   an optional modifier string.
data Rule = Literal String
          | Keyword Char (Maybe String)
    deriving Show

logFormatParser :: String -> Either ParseError (Parser (Map String String))
logFormatParser logFormat = parse rulesParser parserName logFormat
  where rulesParser = do rules <- logFormatSpecParser
                         return $ buildLogRecordParser rules
        parserName = "Parsing LogFormat [" ++ logFormat ++ "]"

{-
  Tokenize the LogFormat string that specifies the grammar
  for log records into a list of Rules.
 -}
logFormatSpecParser = do rules <- many1 (rule <|> literal)
                         return $ combineLiterals rules

combineLiterals [] = []
combineLiterals (Literal l1 : Literal l2 : rs) =
  combineLiterals $ Literal (l1 ++ l2) : rs
combineLiterals (r:rs) = r : combineLiterals rs

-- Parser for a single % rule in the LogFormat string, including %%.
rule = do char '%'
          mod <- optionMaybe modifier
          format <- oneOf "%aABbCDefhHilmnopPqrstTuUvVXIO"
          if mod == (Just ">") && format /= 's'
            then fail $ "The > modifier can only be used with %s but you used it with %" ++ [format]
            else return $ buildResult format mod
  where modifier = (string ">")
        buildResult '%' _ = Literal "%"
        buildResult fmt mod = Keyword fmt mod

literal = do str <- many1 $ noneOf "%"
             return $ Literal str

buildLogRecordParser :: [Rule] -> Parser (Map String String)
buildLogRecordParser rules = combineMapBuilders (Prelude.map parserFor rules) empty

mapAppender map [] = map
mapAppender map (v1:vs) = mapAppender (insert v1 v1 map) vs

-- | Update a map with each of the key-value pairs built by some instances of a
--   Monad. We'll use it to take a bunch of 'Parser's that each parse an
--   optional key-value pair and merge those into a 'Parser' that builds a 'Map'.
combineMapBuilders :: (Monad m, Ord k) =>
                        [m (Maybe (k,v))] -> Map k v -> m (Map k v)
combineMapBuilders [] map = return map
combineMapBuilders (p1:ps) map = do
  result <- p1
  case result of
    Nothing -> combineMapBuilders ps map
    Just (k,v) -> combineMapBuilders ps (insert k v map)

-- | Build a parser for a 'Rule'.
--
--   For 'Keyword' 'Rule's:
--
--   Take a character that is used to define a field in the LogFormat
--   specification and return a 'Parser' that will parse out a key-value
--   for that field from the input. For example, %U in a LogFormat means
--   the URL path, so a URL path parser is available as
--
--   @
--       parserFor (Keyword \'U\' Nothing)
--   @
parserFor :: Rule -> Parser (Maybe (String, String))

-- Build a parser that matches an exact string literal and returns Nothing.
parserFor (Literal lit) = do string lit
                             return Nothing

-- The URL path requested, not including any query string.
parserFor (Keyword 'U' Nothing) = do value <- many1 $ alphaNum <|> char '/'
                                     return $ Just ("path", value)

-- The request method
parserFor (Keyword 'm' Nothing) = do value <- many1 $ oneOf ['A'..'Z']
                                     return $ Just ("method", value)

-- The process ID or thread id of the child that serviced the request.
parserFor (Keyword 'P' Nothing) = do value <- many1 digit
                                     return $ Just ("processId", value)

-- The time taken to serve the request, in seconds.
parserFor (Keyword 'T' Nothing) = do value <- many1 digit
                                     return $ Just ("timeTakenSeconds", value)

-- The time taken to serve the request, in microseconds.
parserFor (Keyword 'D' Nothing) = do value <- many1 digit
                                     return $ Just ("timeTakenMicroseconds", value)

-- Size of response in bytes, excluding HTTP headers.
parserFor (Keyword 'B' Nothing) = do value <- many1 digit
                                     return $ Just ("bytes", value)

-- Size of response in bytes, excluding HTTP headers.
-- In CLF format, i.e. a '-' rather than a 0 when no bytes are sent.
parserFor (Keyword 'b' Nothing) = do value <- (many1 digit) <|> (string "-")
                                     return $ Just ("bytesCLF", value)

-- Remote IP-address
parserFor (Keyword 'a' Nothing) = do value <- sepBy (many1 digit) (string ".")
                                     return $ Just ("remoteIP", foldl1 (\a b -> a ++ "." ++ b) value)

-- Local IP-address
parserFor (Keyword 'A' Nothing) = do value <- sepBy (many1 digit) (string ".")
                                     return $ Just ("localIP", foldl1 (\a b -> a ++ "." ++ b) value)

-- The query string (prepended with a ? if a query string exists, otherwise an empty string)
parserFor (Keyword 'q' Nothing) = do value <- (string "") <|> queryStringParser
                                     return $ Just ("queryString", value)
  where queryStringParser = do char '?'
                               qs <- many1 $ alphaNum <|> char '&' <|> char '='
                               return $ "?" ++ qs

-- Status.
-- For requests that got internally redirected, this is the status of the *original* request,
-- %...>s for the last.
parserFor (Keyword 's' mod) = do value <- many1 alphaNum
                                 return $ Just (format mod, value)
  where format Nothing = "statusOriginal"
        format (Just ">") = "statusLast"
