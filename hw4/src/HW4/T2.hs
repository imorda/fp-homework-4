{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Scientific
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState (..))
import HW4.Types

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES runA)) input = case runA (0, input) of
  Error e          -> Error e
  Success (a :# _) -> Success a

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

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

pEof :: Parser ()
pEof = P $ ES $ \input@(pos, s) ->
  case s of
    [] -> Success (() :# input)
    _  -> Error (ErrorAtPos pos)

pExpect :: Char -> Parser Char
pExpect x = mfilter (== x) pChar

pDigits :: Num a => Parser [a]
pDigits = do
  digitStr <- some $ mfilter isDigit pChar
  return $ map (fromIntegral . digitToInt) digitStr

pInteger :: Parser Integer
pInteger = pDigits >>= \digits ->
  return $ foldl1 (\a b -> a * 10 + b) digits

pIntegerCounting :: Parser (Integer, Natural)
pIntegerCounting = pDigits >>= \digits ->
  return $ foldl (\(a, counter) b -> (a * 10 + b, counter + 1)) (0, 0) digits

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

pSkipWs :: Parser ()
pSkipWs = void $ many $ mfilter isSpace pChar

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

parseE :: Parser Expr
parseE = do
  lhs <- parseT
  parseE' lhs

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

parseT :: Parser Expr
parseT = do
  lhs <- parseF
  parseT' lhs


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

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  parsed <- parseE
  pEof
  return parsed
