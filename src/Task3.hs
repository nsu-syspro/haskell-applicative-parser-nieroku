{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.List (intercalate)
import Parser
import ParserCombinators
import Prelude hiding (exponent)

-- | JSON representation
--
-- See <https://www.json.org>
data JValue
  = JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
  deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
json :: Parser JValue
json = whitespace *> choice [jobject, jarray, jstring, jnumber, jbool, jnull]

jobject :: Parser JValue
jobject =
  char '{'
    *> choice
      [ sepBy1 (whitespace *> keyValue <* whitespace) (char ','),
        whitespace $> []
      ]
    <* char '}'
    <&> JObject
  where
    keyValue = (,) <$> jsonstring <* whitespace <* char ':' <* whitespace <*> json

jarray :: Parser JValue
jarray =
  char '['
    *> choice
      [ sepBy1 (whitespace *> json <* whitespace) (char ','),
        whitespace $> []
      ]
    <* char ']'
    <&> JArray

jstring :: Parser JValue
jstring = JString <$> jsonstring

jnumber :: Parser JValue
jnumber = JNumber . read . concat <$> sequenceA [sign, integer, fraction, exponent]
  where
    sign = option "" (string "-")
    integer = choice [string "0", (:) <$> nonZeroDigit <*> many (digit)]
    fraction = option "" ((:) <$> char '.' <*> some digit)
    exponent = option "" (concat <$> sequenceA [choice [string "e", string "E"], choice [string "+", string "-", string ""], some digit])

jbool :: Parser JValue
jbool = choice [string "false" $> JBool False, string "true" $> JBool True]

jnull :: Parser JValue
jnull = string "null" $> JNull

whitespace :: Parser ()
whitespace = many (satisfy (flip elem [' ', '\n', '\r', '\t'])) $> ()

jsonstring :: Parser String
jsonstring = char '"' *> (concat <$> many (choice [character, escapeSequence, uEscapeSequence])) <* char '"'
  where
    character = (: "") <$> satisfy (not . \c -> isControl c || c `elem` ['"', '\\'])

    -- escapeSequences :: [(Char, Char)]
    -- escapeSequences =
    --   [ ('"', '"'),
    --     ('\\', '\\'),
    --     ('/', '/'),
    --     ('b', '\b'),
    --     ('f', '\f'),
    --     ('n', '\n'),
    --     ('r', '\r'),
    --     ('t', '\t')
    --   ]
    -- escapeSequence = (\a b -> [a, b]) <$> char '\\' <*> choice (map (\(code, c) -> char code $> c) escapeSequences)

    escapeSequence = (choice . map string) ["\\\"", "\\\\", "\\/", "\\b", "\\f", "\\n", "\\r", "\\t"]

    hexDigitToInt c
      | '0' <= c && c <= '9' = ord c - ord '0'
      | 'a' <= c && c <= 'f' = 10 + ord c - ord 'a'
      | 'A' <= c && c <= 'F' = 10 + ord c - ord 'A'
      | otherwise = undefined
    readHex = foldl' (\acc d -> acc * 0x10 + hexDigitToInt d) 0 :: String -> Int
    uEscapeSequence = (++) <$> string "\\u" <*> count 4 hexDigit <&> ((: "") . chr . readHex)

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull = ["null"]
renderTokens (JBool b) = [map toLower $ show b]
renderTokens (JNumber d) = [show d]
renderTokens (JString s) = ["\"" ++ s ++ "\""]
renderTokens (JArray xs) = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
  where
    renderPair :: (String, JValue) -> [String]
    renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
