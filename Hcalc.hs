{-# LANGUAGE GADTs, FlexibleInstances, OverloadedStrings,
    ExistentialQuantification, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Spreadsheet functionality in haskell
module Hcalc where

import Data.String
import Text.Printf
default (Cell')

-- * Sheets and operations

-- | A spreadsheet is a list of columns
newtype Sheet = Sheet [Col]

--instance IsList Sheet -- :3 (-XOverloadedLists (requires GHC HEAD atm)

data Header

class IsCell cell where
    cappend :: Col -> cell -> Col

instance IsCell (Cell c) where
    cappend ColEmpty   c  = Col c ColEmpty
    cappend (Col c cs) c' = Col c (cappend cs c')

instance IsCell Cell' where
    cappend col (Cell' c) = cappend col c

(><) :: IsCell c => Col -> c -> Col
(><) = cappend

instance Show Sheet where
    show (Sheet (x:xs)) = foldl (\s n -> s ++ showRow n ++ "\n") ""
                                [0 .. maximum (map colLength (x:xs)) - 1]
        where
            showRow :: Int -> String
            showRow n = foldl (\s col -> s ++ " | " ++ show (cellAt n col))
                              (show $ cellAt n x)
                              xs

colNames :: String
colNames = ['A' .. 'Z']

-- * Formulas

data CR = CR Int Int

crRange :: CR -> CR -> [CR]
crRange (CR x y) (CR x' y') = [CR a b | a <- [x..x'], b <- [y..y'] ]

unRef :: Sheet -> CR -> Double
unRef (Sheet cols) (CR c r) = case cellAt r (cols !! c) of
    Cell' (CellDouble d) -> d
    _ -> error "unRef: only doubles are supported as refs"

data Formula a where
    Frf  :: CR -> Formula CR
    Fnum :: Double -> Formula Double
    FDiv :: Formula Double -> Formula Double -> Formula Double
    FAdd :: Formula Double -> Formula Double -> Formula Double
    FSum :: CR -> CR -> Formula Double

instance Show (Formula a) where
    show _ = "$formula$"

feval :: Sheet -> Formula a -> a
feval _ (Frf cr) = cr
feval _ (Fnum n) = n
feval s (FDiv a b) = feval s a / feval s b
feval s (FAdd a b) = feval s a + feval s b
feval s (FSum a b) = sum $ map (unRef s) $ crRange a b

-- * Cells

data Cell a where
    CellEmpty   :: Cell ()
    CellHeader  :: String    -> Cell Header
    CellString  :: String    -> Cell String
    CellInteger :: Integer   -> Cell Integer
    CellDouble  :: Double    -> Cell Double
    CellFormula :: Show b => Formula b -> Cell (Formula b)

instance Show (Cell a) where
    show CellEmpty       = printf "%20s" ("." :: String)
    show (CellHeader  h) = printf "%20s" $ "[" ++ h ++ "]"
    show (CellString  s) = printf "%20s" s
    show (CellInteger i) = printf "%20s" (show i)
    show (CellDouble  d) = printf "%20f" d
    show (CellFormula f) = printf "%20s" (show f)

-- | wrapper
data Cell' = forall a. Cell' (Cell a)
instance Show Cell' where
    show (Cell' c) = show c

instance IsString Cell' where
    fromString (':':':':' ':s) = Cell' $ CellHeader s
    fromString s               = Cell' $ CellString s

cellInteger :: Integer -> Cell'
cellInteger = Cell' . CellInteger

cellDouble :: Double -> Cell'
cellDouble = Cell' . CellDouble

cellFormula :: Show a => Formula a -> Cell'
cellFormula = Cell' . CellFormula

-- * Columns

data Col = ColEmpty
         | forall a. Col (Cell a) Col

instance Show Col where
    show ColEmpty   = ""
    show (Col c cs) = show c ++ show cs

-- | construct a column with header
hcol :: Cell' -> [Cell'] -> Col
hcol (Cell' hc) (Cell' c : cs) = Col hc $ foldl
    (\col (Cell' c') -> col `cappend` c') (Col c ColEmpty) cs

cellAt :: Int -> Col -> Cell'
cellAt 0 (Col c   _) = Cell' c
cellAt n (Col _ col) = cellAt (n-1) col
cellAt _ ColEmpty    = Cell' CellEmpty

colLength :: Col -> Int
colLength = colLength' 0
    where colLength' n ColEmpty = n
          colLength' n (Col _ cs) = colLength' (n + 1) cs

-- * Examples

exampleFormula :: Formula Double
exampleFormula = FAdd (Fnum 2) (Fnum 5) 

--       , cellFormula exampleFormula

t1 :: Sheet
t1 = Sheet
   [ hcol ":: What?"
        [ "ab-lippu"     
        , "Wombats kalja"
        , "Sushi"
        , "8mm tequila"
        , "Yesterday"
        , "Taxi"
        , "Aamiainen"
        , "Pumpattava octo"
        , "Sampe maksaa"
        ]
   , hcol ":: Matti-Sampe"
       [ cellDouble    2.1
       , cellDouble (- 2.8)
       , cellDouble    9
       , cellDouble (- 2)
       , cellDouble (- 4.5)
       , cellDouble   11.5
       , cellDouble (- 7.5)
       , cellFormula (FSum (CR 1 1) (CR 1 7))
       ]
   ]

main :: IO ()
main = print t1
