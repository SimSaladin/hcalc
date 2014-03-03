{-# LANGUAGE OverloadedStrings #-}

module Fdsl(formula) where

import Hcalc

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.Char     (ord)
import Control.Monad (liftM)


-- | This is used to get some basic parsers

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef
  where 
    languageDef =
        emptyDef { reservedOpNames = ["+", "-", "*", "/"]
                 , reservedNames   = ["sum"]
                 }

reserved  :: String -> Parser ()
reservedOp:: String -> Parser ()
parens    :: Parser a -> Parser a
float     :: Parser Double
integer   :: Parser Integer
natural   :: Parser Integer
whiteSpace:: Parser ()

reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
float      = Token.float      lexer
integer    = Token.integer    lexer
natural    = Token.natural    lexer
whiteSpace = Token.whiteSpace lexer



-- | Main expression parser (Double)
dExpression :: Parser (Formula Double)
dExpression = buildExpressionParser basicOperators dTerm

basicOperators :: OperatorTable Char () (Formula Double)
basicOperators = [ [prefix "-" FNeg, prefix "+" id]
                 , [binary "*" FMul AssocLeft, binary "/" FDiv AssocLeft]
                 , [binary "+" FAdd AssocLeft, binary "-" FSub AssocLeft]
                 ]
 
binary :: String -> (a -> a -> a) -> Assoc -> Operator Char () a
prefix :: String -> (a -> a) -> Operator Char () a 

binary  name fun assoc = Infix   (reservedOp name >> return fun) assoc
prefix  name fun       = Prefix  (reservedOp name >> return fun)
--postfix  name fun       = Postfix (reservedOp name >> return fun)

-- | Double term parser
dTerm :: Parser (Formula Double)
dTerm =  try function
     <|> parens dExpression
     <|> liftM FnumCR cellRef -- Constant from a cell
     <|> liftM Fnum constant  -- Literal constant
     where constant = try float <|> liftM fromInteger integer

function :: Parser (Formula Double)
function = parens (reserved "sum" >> liftM FSum cellRange) 
    -- <|> 
    -- TODO: Write some kind of function list, for easy function adding

-- | Parse excel like column string (containing chars ['A'..'Z']) to Int starting from 1. 
col :: Parser Int
col = let letters = many1 $ oneOf ['A' .. 'Z']
          toInt :: String -> Int
          toInt str = sum $ zipWith (\chr n -> (ord chr - 64) * (26 ^ n)) str ([0..] :: [Int])
      in liftM toInt $ letters

-- | <columnletters><rownumbers>
cellRef :: Parser CR
cellRef = do
    column <- col
    row    <- natural
    return $ CR column (fromInteger row)

-- | <CR>:<CR>
cellRange :: Parser (CR,CR)
cellRange = do
    a <- cellRef
    char ':'
    b <- cellRef
    return (a,b)


-- * Parse formula expression
formula :: String -> Formula Double
formula s = case parse (whiteSpace >> dExpression) "(formula)" s of
    Right f -> f
    Left e -> error . show $ e

