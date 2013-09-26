{-# LANGUAGE GADTs, FlexibleInstances, OverloadedStrings,
    ExistentialQuantification #-}
module Hcalc where

import Data.String

-- * Sheets and operations

newtype Sheet = Sheet [Col]

data Header

data CR = CR Int Int

(><) :: Col -> Cell b -> Col
col >< cell = colAppend col cell
    where colAppend ColEmpty   c  = Col c ColEmpty
          colAppend (Col c cs) c' = Col c (colAppend cs c')

instance Show Sheet where
    show (Sheet (x:xs)) = foldl (\s n -> s ++ showRow n ++ "\n") ""
                                [0 .. maximum (map colLength (x:xs)) - 1]
        where
            showRow :: Int -> String
            showRow n = foldl (\s col -> s ++ " | " ++ show (cellAt n col))
                              (show $ cellAt n x)
                              xs

colNames :: [Char]
colNames = ['A' .. 'Z']


-- * Formulas

data Formula a where
    Frf  :: CR -> Formula CR
    Fnum :: Double -> Formula Double
    FDiv :: Formula Double -> Formula Double -> Formula Double
    FAdd :: Formula Double -> Formula Double -> Formula Double

feval :: Formula a -> a
feval (Frf cr) = cr
feval (Fnum n) = n
feval (FDiv a b) = feval a / feval b
feval (FAdd a b) = feval a + feval b

-- * Cells

data Cell a where
    CellEmpty   :: Cell ()
    CellHeader  :: String    -> Cell Header
    CellString  :: String    -> Cell String
    CellInteger :: Integer   -> Cell Integer
    CellFormula :: Show b => Formula b -> Cell (Formula b)

instance IsString (Cell String) where
    fromString = CellString

instance Show (Cell a) where
    show CellEmpty       = "n/a"
    show (CellHeader  h) = "[" ++ h ++ "]"
    show (CellString  s) = s
    show (CellInteger i) = show i
    show (CellFormula f) = show (feval f)

data Cell' = forall a. Cell' (Cell a)
instance Show Cell' where
    show (Cell' c) = show c

-- * Columns

data Col = ColEmpty
         | forall a. Col (Cell a) Col

cellAt :: Int -> Col -> Cell'
cellAt 0 (Col c   _) = Cell' c
cellAt n (Col _ col) = cellAt (n-1) col
cellAt _ ColEmpty    = Cell' CellEmpty

colLength :: Col -> Int
colLength = colLength' 0
    where colLength' n ColEmpty = n
          colLength' n (Col _ cs) = colLength' (n + 1) cs

fcol :: Cell a -> Col
fcol cell = Col cell ColEmpty

instance Show Col where
    show ColEmpty   = ""
    show (Col c cs) = show c ++ show cs
exampleFormula :: Formula Double
exampleFormula = FAdd (Fnum 2) (Fnum 5) 

t1 :: Sheet
t1 = Sheet
     [ fcol (CellHeader "What?")
       ><  ("ab-lippu"      :: Cell String)
       ><  ("Wombats kalja" :: Cell String)

     , fcol (CellHeader "Matti")
       ><  CellInteger 2
       >< CellFormula exampleFormula
     ]

main :: IO ()
main = do
    print t1
    putStrLn "done"
