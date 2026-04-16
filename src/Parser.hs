{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note

    --

    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser,
    parse,
    parseMaybe,
    satisfy,
    Error (..),
    Position (..),
    Parsed (..),
    Input,
  )
where

import Control.Applicative
import Control.Arrow
import Data.List

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
  deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error
  = -- | Unexpected character
    Unexpected Char
  | -- | Unexpected end of input
    EndOfInput
  deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a
  = -- | Successfully parsed value of type @a@ with remaining input to be parsed
    Parsed a Input
  | -- | Failed to parse value of type @a@ with accumulated list of errors
    Failed [Position Error]
  deriving (Show)

instance Functor Parsed where
  fmap f (Parsed a input) = Parsed (f a) input
  fmap _ (Failed errors) = Failed errors

-- | Parser of value of type @a@
newtype Parser a = Parser {runParser :: Input -> Parsed a}

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse parser = runParser parser . Position 0

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser input = case parse parser input of
  (Parsed a _) -> Just a
  _ -> Nothing

instance Functor Parser where
  fmap = liftA

instance Applicative Parser where
  pure = Parser . Parsed
  (Parser a) <*> (Parser b) = Parser runParser
    where
      runParser input = case a input of
        (Parsed f input') -> f <$> b input'
        (Failed errors) -> Failed errors

instance Alternative Parser where
  empty = Parser runParser
    where
      runParser (Position pos "") = Failed [Position pos EndOfInput]
      runParser (Position pos (c : _)) = Failed [Position pos (Unexpected c)]

  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (Parser a) <|> (Parser b) = Parser runParser
    where
      runParser input = case (a &&& b) input of
        (parsed@(Parsed _ _), _) -> parsed
        (Failed l, Failed r) -> Failed (nub (l ++ r))
        (_, r) -> r

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser runParser
  where
    runParser (Position pos "") = Failed [Position pos EndOfInput]
    runParser (Position pos (c : cs))
      | p c = Parsed c (Position (pos + 1) cs)
      | otherwise = Failed [Position pos (Unexpected c)]
