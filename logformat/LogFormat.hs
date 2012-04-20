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
--   exactly) and % directives that match a certain pattern.
data Rule = Literal String
          | Keyword Char
    deriving Show

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

rule = do char '%'
          format <- oneOf "%aABbCDefhHilmnopPqrstTuUvVXIO"
          return $ if format == '%' then Literal "%"
                                    else Keyword format

literal = do str <- many1 $ noneOf "%"
             return $ Literal str

--buildLogRecordParser :: [Rule] -> Parser (Map String String)

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

-- | Build a parser that parser an exact string literal and returns Nothing.
literalParser literal = do string literal
                           return Nothing

-- | Take a character that is used to define a field in the LogFormat
--   specification and return a 'Parser' that will parse out a key-value
--   for that field from the input. For example, %U in a LogFormat means
--   the URL path, so a URL path parser is available as
--
--   @
--       parserFor \'U\'
--   @
parserFor :: Char -> Parser (Maybe (String, String))

-- The URL path requested, not including any query string.
parserFor 'U' = do value <- many1 $ alphaNum <|> char '/'
                   return $ Just ("path", value)
