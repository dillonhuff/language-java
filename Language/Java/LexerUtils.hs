module Language.Java.LexerUtils() where

import Numeric
import Data.Char
import Data.Either

type LexError a = Either String a

pickyReadOct :: String -> Integer
pickyReadOct s =
  if not $ null rem
  then lexicalError $ "Non-octal digit '" ++ take 1 rem ++ "' in \"" ++ s ++ "\"."
  else n
    where (n,rem) = head $ readOct s

readHexExp :: (Floating a, Eq a) => String -> a
readHexExp s = let (m, suf) = head $ readHex s
                   (e, _) = case suf of
                             p:s | toLower p == 'p' -> head $ readHex s
                             _ -> (0, "")
                in m ** e

readCharTok :: String -> Char
readCharTok s = head . convChar . dropQuotes $ s

readStringTok :: String -> String
readStringTok = convChar . dropQuotes

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  case all isHexDigit [d1,d2,d3,d4] of
    True -> toEnum (read ['0','x',d1,d2,d3,d4]):convChar s'
    False -> lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  case isOctDigit c of
    True -> convOctal maxRemainingOctals
    False -> (escChar c):(convChar s)
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in toChar (c:octals):convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = x:convChar s
convChar "" = ""

escChar c = case c of
  'b' -> '\b'
  'f' -> '\f'
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  '\'' -> '\''
  '\\' -> '\\'
  '"' -> '"'
  _ -> lexicalError $ "bad escape \"\\" ++ (c : "\"")
  

lexicalError :: String -> a
lexicalError = error . ("DILLON LEXER lexical error: " ++)

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)
