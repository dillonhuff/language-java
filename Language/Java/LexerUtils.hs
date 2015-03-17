module Language.Java.LexerUtils(L(..),
                                Pos(..),
                                pickyReadOct,
                                readHexExp,
                                readStringTok,
                                readCharTok) where

import Control.Monad
import Data.Char
import Data.Either
import Numeric

type LexError a = Either String a

pickyReadOct :: String -> LexError Integer
pickyReadOct s =
  if not $ null rem
     then lexicalError $ "Non-octal digit '" ++ take 1 rem ++ "' in \"" ++ s ++ "\"."
     else return n
    where (n,rem) = head $ readOct s

readHexExp :: (Floating a, Eq a) => String -> a
readHexExp s = let (m, suf) = head $ readHex s
                   (e, _) = case suf of
                             p:s | toLower p == 'p' -> head $ readHex s
                             _ -> (0, "")
                in m ** e

readCharTok :: String -> LexError Char
readCharTok s = liftM head $ convChar . dropQuotes $ s

readStringTok :: String -> LexError String
readStringTok str = convChar $ dropQuotes str

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> LexError String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  case all isHexDigit [d1,d2,d3,d4] of
    True -> do
      res <- convChar s'
      return $ toEnum (read ['0','x',d1,d2,d3,d4]):res -- convChar s'
    False -> lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  case isOctDigit c of
    True -> convOctal maxRemainingOctals
    False -> do
      res <- convChar s
      eChar <- escChar c
      return $ eChar:res -- (convChar s)
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in
           do
             res <- convChar (drop noctals s)
             return $ toChar (c:octals):res  --convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = do
  res <- convChar s
  return $ x:res --convChar s
convChar "" = return ""

escChar :: Char -> LexError Char
escChar c = case c of
  'b' -> return '\b'
  'f' -> return '\f'
  'n' -> return '\n'
  'r' -> return '\r'
  't' -> return '\t'
  '\'' -> return '\''
  '\\' -> return '\\'
  '"' -> return '"'
  _ -> lexicalError $ "bad escape \"\\" ++ (c : "\"")
  

lexicalError :: String -> LexError a
lexicalError str = fail ("DILLON LEXER lexical error: " ++ str)

data L a = L Pos a
  deriving (Show, Eq)

type Line = Int
type Column = Int
type Pos = (Line, Column)
