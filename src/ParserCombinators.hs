{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module ParserCombinators where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Parser

-- | Parses single character
--
-- Usage example:
--
-- >>> parse (char 'b') "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (char 'b') "abc"
-- Failed [Position 0 (Unexpected 'a')]
char :: Char -> Parser Char
char = satisfy . (==)

-- | Parses given string
--
-- Usage example:
--
-- >>> parse (string "ba") "bar"
-- Parsed "ba" (Position 2 "r")
-- >>> parse (string "ba") "abc"
-- Failed [Position 0 (Unexpected 'a')]
string :: String -> Parser String
string = traverse char

-- | Skips zero or more space characters
--
-- Usage example:
--
-- >>> parse spaces "  bar"
-- Parsed () (Position 2 "bar")
-- >>> parse spaces "bar"
-- Parsed () (Position 0 "bar")
-- >>> parse (spaces *> string "bar") "bar"
-- Parsed "bar" (Position 3 "")
spaces :: Parser ()
spaces = many (char ' ') $> ()

-- | Tries to consecutively apply each of given list of parsers until one succeeds.
-- Returns the *first* succeeding parser as result or 'empty' if all of them failed.
--
-- Usage example:
--
-- >>> parse (choice [char 'a', char 'b']) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (choice [char 'a', char 'b']) "foo"
-- Failed [Position 0 (Unexpected 'f')]
-- >>> parse (choice [string "ba", string "bar"]) "bar"
-- Parsed "ba" (Position 2 "r")
choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = asum

-- Discover and implement more useful parser combinators below
--
-- - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
-- - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>

option :: a -> Parser a -> Parser a
option a p = p <|> pure a

optionM :: (Monoid m) => Parser m -> Parser m
optionM = option mempty

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 a sep = (:) <$> a <*> many (sep *> a)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy a sep = optionM (sepBy1 a sep)

count :: Int -> Parser a -> Parser [a]
count = replicateM

block :: Char -> Char -> Parser a -> Parser a
block o c p = char o *> p <* char c

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> isDigit c && c /= '0')

digit :: Parser Char
digit = satisfy isDigit

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem
