{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.List (intercalate, singleton)
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
jobject = block "{" "}" (JObject <$> sepBy keyValue (whitespace <* char ',') <* whitespace)
  where
    keyValue = whitespace *> pure (,) <*> jsonString <* whitespace <* char ':' <*> json

jarray :: Parser JValue
jarray = block "[" "]" (JArray <$> sepBy json (whitespace *> char ',') <* whitespace)

jstring :: Parser JValue
jstring = JString <$> jsonString

jnumber :: Parser JValue
jnumber = JNumber . read . concat <$> sequenceA [sign, integer, fraction, exponent]
  where
    sign = optionM (string "-")
    integer = choice [string "0", (:) <$> nonZeroDigit <*> many (digit)]
    fraction = optionM ((:) <$> char '.' <*> some digit)
    exponent = optionM $ concat <$> sequenceA [singleton <$> oneOf "eE", optionM (singleton <$> oneOf "+-"), some digit]

jbool :: Parser JValue
jbool = choice [string "false" $> JBool False, string "true" $> JBool True]

jnull :: Parser JValue
jnull = string "null" $> JNull

whitespace :: Parser ()
whitespace = many (satisfy (flip elem [' ', '\n', '\r', '\t'])) $> ()

jsonString :: Parser String
jsonString = block "\"" "\"" $ concat <$> many (choice [character, escapeSequence, uEscapeSequence])
  where
    shouldBeEscaped c = isControl c || c `elem` ['"', '\\']

    character = singleton <$> satisfy (not . shouldBeEscaped)
    escapeSequence = (choice . map string) ["\\\"", "\\\\", "\\/", "\\b", "\\f", "\\n", "\\r", "\\t"]
    uEscapeSequence = (++) <$> string "\\u" <*> count 4 hexDigit

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
