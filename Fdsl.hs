{-# LANGUAGE OverloadedStrings #-}

module Fdsl where

import Hcalc
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many, optional)

import Data.Char     (ord)
import Control.Monad (liftM)
import Numeric (readSigned, readFloat)


-- | Parse excel like column string (containing chars ['A'..'Z']) to Int starting from 1. 
pLetterInt :: Parser Int
pLetterInt = let letters = many1 $ oneOf ['A' .. 'Z']
                 toInt :: String -> Int
                 toInt str = sum $ zipWith (\chr n -> (ord chr - 64) * (26 ^ n)) str ([0..] :: [Int])
             in liftM toInt $ letters

pInt :: (Integral a, Read a) => Parser a
pInt = many1 digit >>= return . read

ws :: Parser String
ws = many $ oneOf " "

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

pParentheses :: Parser a -> Parser a
pParentheses p = char '(' *> p <* char ')'

-- | Parse double (as in Real World Haskell, p. 400)
pDouble :: Parser (Formula Double)
pDouble = do
    s <- getInput
    case readSigned readFloat s of
        [(n,s')]  -> Fnum <$> (pure n <* setInput s')
        _         -> fail "Error: Couldn't parse Double"


pFDouble :: Parser (Formula Double)
pFDouble = pSimpleCalc
        <||> pSum -- TODO: pFunc
        <||> (FnumCR <$> pCR) 
        <||> pDouble


pFLeftDouble :: Parser (Formula Double)
pFLeftDouble = pSum -- TODO: pFunc
        <||> (FnumCR <$> pCR) 
        <||> pDouble


-- * Top-level Formula parsers


-- | <columnletters><rownumbers>
pCR :: Parser CR
pCR = do
    columns <- pLetterInt
    rows    <- pInt
    return $ CR columns rows

-- | <CR>:<CR>
pCRRange :: Parser (CR,CR)
pCRRange = do
    a <- pCR
    char ':'
    b <- pCR
    return (a,b)

-- | Template for simple calculations (+, /, *, etc.) with syntax: <FDouble> <symbol> <FDouble>
simpleCalcParser :: Char 
                 -> (Formula Double -> Formula Double -> Formula Double)
                 -> Parser (Formula Double)
simpleCalcParser symbol func = do
   a <- pFLeftDouble
   lexeme $ char symbol
   b <- pFDouble
   return $ func a b

--pAdd = FAdd <$> (pure pFDouble <* lexeme $ char '+') <*> pFDouble

pAdd :: Parser (Formula Double)
pAdd = simpleCalcParser '+' FAdd

pSub :: Parser (Formula Double)
pSub = simpleCalcParser '-' FSub

pMul :: Parser (Formula Double)
pMul = simpleCalcParser '*' FMul

pDiv :: Parser (Formula Double)
pDiv = simpleCalcParser '/' FDiv

pSimpleCalc :: Parser (Formula Double)
pSimpleCalc = pAdd <||> pSub <||> pMul <||> pDiv

-- | (sum <CRRange>)
pSum :: Parser (Formula Double)
pSum = (uncurry FSum) <$> pParentheses (string "sum" *> ws *> pCRRange)


--pFormula :: Parser (Formula Double)
--pFormula = 



formula :: String -> Formula Double
formula s = case parse pFDouble "(formula)" s of
    Right f -> f
    Left e -> error . show $ e

