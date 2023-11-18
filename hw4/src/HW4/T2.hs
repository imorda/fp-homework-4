{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  (
  -- * Type
  -- | Custom data types encapsulating the parser information.
    ParseError (..),
    Parser,

  -- * Parsers
  -- | Functions exporting combinatory parsing techniques which can also fail with 'ParseError'.
    runP,
    pChar,
    parseError,
    parseExpr,
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Scientific
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState (..))
import HW4.Types

-- | Captures any parsing error along with the position of
--   the error in the input.
newtype ParseError = ErrorAtPos Natural
  deriving Show

-- | 'Parser' is a simple wrapper over 'ExceptState' with 'ParseError'
--   as the error type, meaning that any computation made in the
--   'Parser' monad has the ability to fail with a 'ParseError'.
--   It models a computation that consumes a string and produces a value,
--   and it can either succeed, returning the parsed value and
--   the rest of the string, or fail with an error.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Runs a parser on a given input string. If parsing is successful, the result is returned,
-- otherwise an error is thrown.
runP :: Parser a -> String -> Except ParseError a
runP (P (ES runA)) input = case runA (0, input) of
  Error e          -> Error e
  Success (a :# _) -> Success a

-- | Parses a single character. If parsing is successful, the character is returned,
-- otherwise an error is thrown.
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | A parser that always fails with an ErrorAtPos error at the current position.
parseError :: Parser a
parseError = P $ ES $ \(pos, _) ->
  Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P (ES runA)) <|> (P (ES runB)) = P $ ES $ \x ->
    case runA x of
       Error _     -> runB x
       Success ret -> Success ret

instance MonadPlus Parser

-- | Expects the end of file (EOF) in the input. If EOF is found, a unit (()) is returned,
-- otherwise an error is thrown.
pEof :: Parser ()
pEof = P $ ES $ \input@(pos, s) ->
  case s of
    [] -> Success (() :# input)
    _  -> Error (ErrorAtPos pos)

-- | Expects a specific character in the input. If the character is found, it's returned,
-- otherwise an error is thrown.
pExpect :: Char -> Parser Char
pExpect x = mfilter (== x) pChar

-- | Parses a sequence of digits.
pDigits :: Num a => Parser [a]
pDigits = do
  digitStr <- some $ mfilter isDigit pChar
  return $ map (fromIntegral . digitToInt) digitStr

-- | Parses an integer number.
pInteger :: Parser Integer
pInteger = pDigits >>= \digits ->
  return $ foldl1 (\a b -> a * 10 + b) digits

-- | Parses an integer and counts the number of digits it contains.
pIntegerCounting :: Parser (Integer, Natural)
pIntegerCounting = pDigits >>= \digits ->
  return $ foldl (\(a, counter) b -> (a * 10 + b, counter + 1)) (0, 0) digits

-- | Parses a double number. A double is represented as an integer part and a decimal part.
pDouble :: Parser Double
pDouble = do
  digitsInt <- pInteger
  digitsFrac <- optional $ do
    void $ pExpect '.'
    pIntegerCounting
  let (fracInt, fracLength) = fromMaybe (0, 0) digitsFrac
  return $ toRealFloat $ scientific
    (digitsInt * (10 ^ fracLength) + fracInt)
    $ - fromIntegral fracLength

-- | Skips all whitespace characters in the input.
pSkipWs :: Parser ()
pSkipWs = void $ many $ mfilter isSpace pChar

-- Grammar schema
-- E -> TE'
-- E' -> S+STE'
-- E' -> S-STE'
-- E' -> eps

-- T -> FT'
-- T' -> S*SFT'
-- T' -> S/SFT'
-- T' -> eps

-- F -> SnumberS
-- F -> S(E)S

-- S -> ⎵S
-- S -> eps

-- | This function is used for representing the grammar of a parser,
-- representing expression
parseE :: Parser Expr
parseE = do
  lhs <- parseT
  parseE' lhs

-- | This unction is used for representing the grammar of a parser,
-- representing expression part
parseE' :: Expr -> Parser Expr
parseE' lhs =
  (do
    pSkipWs
    op <- pExpect '+' <|> pExpect '-'
    pSkipWs
    if op == '+'
      then Op . Add lhs <$> parseT
      else Op . Sub lhs <$> parseT) <|>
  return lhs

-- | This function is used for representing the grammar of a parser,
-- representing term
parseT :: Parser Expr
parseT = do
  lhs <- parseF
  parseT' lhs


-- | This function is used for representing the grammar of a parser,
-- representing term part
parseT' :: Expr -> Parser Expr
parseT' lhs =
  (do
    pSkipWs
    op <- pExpect '*' <|> pExpect '/'
    pSkipWs
    if op == '*'
      then Op . Mul lhs <$> parseF
      else Op . Div lhs <$> parseF) <|>
  return lhs

-- | Parses a factor. A factor is either a number or an expression within parentheses.
parseF :: Parser Expr
parseF =
  (do
    pSkipWs
    void $ pExpect '('
    innerE <- parseE
    void $ pExpect ')'
    pSkipWs
    return innerE) <|>
  (do
    pSkipWs
    n <- pDouble
    pSkipWs
    return $ Val n)

-- | 'parseExpr' parses a mathematical expression represented as a string into an 'Expr' data structure.
-- If the parsing is successful, an 'Expr' is returned, otherwise an error is thrown.
parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  parsed <- parseE
  pEof
  return parsed
