{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

import Control.Applicative
import Data.Functor
import Parser
import ParserCombinators

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day = Day Int deriving (Show, Eq)

newtype Month = Month Int deriving (Show, Eq)

newtype Year = Year Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]
  where
    dotFormat = Date <$> day <* char '.' <*> month <* char '.' <*> year
    hyphenFormat = Date <$> day <* char '-' <*> month <* char '-' <*> year
    usFormat = flip Date <$> monthName <* char ' ' <*> usDay <* char ' ' <*> year

    usDay =
      Day . read
        <$> choice
          [ sequenceA [char '1', digit],
            sequenceA [char '2', digit],
            string "30",
            string "31",
            sequenceA [nonZeroDigit]
          ]
    day =
      Day . read
        <$> choice
          [ sequenceA [char '0', digit],
            sequenceA [char '1', digit],
            sequenceA [char '2', digit],
            string "30",
            string "31"
          ]
    month =
      Month . read
        <$> choice
          [ sequenceA [char '0', nonZeroDigit],
            string "10",
            string "11",
            string "12"
          ]
    year = Year . read <$> some digit

    monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    monthName = Month <$> choice (map (\(i, name) -> string name $> i) (zip [1 ..] monthNames))
