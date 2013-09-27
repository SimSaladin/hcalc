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

instance Show Sheet where
    show = flip sheetShowHelper show

sheetShowHelper :: Sheet -> (Cell' -> String) -> String
sheetShowHelper (Sheet (x:xs)) showCell = foldl
    (\s n -> s ++ showRow n ++ "\n") ""
    [0 .. maximum (map colLength (x:xs)) - 1]
    where showRow n = foldl (\s col -> s ++ " | " ++ showCell (cellAt n col))
                            (showCell $ cellAt n x)
                            xs

showEval :: Sheet -> String
showEval sheet = sheetShowHelper sheet (showCellEval sheet)

colNames :: String
colNames = ['A' .. 'Z']

-- | Execute a transformation on each column
eachCol :: Sheet -> (Int -> Col -> Col) -> Sheet
eachCol (Sheet xs) f = Sheet (zipWith f [0..] xs)

-- insRow :: Int -> [Cell'] -> Sheet -> Sheet

-- | Print a sheet with a title
printWithTitle :: Sheet -> String -> IO ()
printWithTitle sheet s = do
    putStrLn $ "\n----- " ++ s ++ " -----\n"
    putStrLn (showEval sheet)

-- * Refs and formulas

data CR = CR Int Int

crRange :: CR -> CR -> [CR]
crRange (CR x y) (CR x' y') = [CR a b | a <- [x..x'], b <- [y..y'] ]

unRef :: Sheet -> CR -> Double
unRef sheet@(Sheet cols) (CR c r) = case cellAt r (cols !! c) of
    Cell' (CellDouble d)  -> d
    Cell' (CellInteger d) -> fromIntegral d
    Cell' (CellFormula f) -> feval sheet f
    Cell' (CellFormulaInt f) -> fromIntegral $ feval sheet f
    Cell' (CellString _)  -> 1 -- FIXME: hax to generalize FSumIf
    Cell' CellEmpty       -> 0
    _ -> error "unRef: only doubles, formula doubles and empties as 0s are supported as refs"

unRefS :: Sheet -> CR -> String
unRefS (Sheet cols) (CR c r) = case cellAt r (cols !! c) of
    Cell' (CellString s) -> s
    Cell' CellEmpty      -> ""
    _                    -> error "unRefS: only strings accepted in cells"

data Formula a where
    Frf  :: CR -> Formula CR
    Fnum :: Double -> Formula Double
    FDiv :: Formula Double -> Formula Double -> Formula Double
    FAdd :: Formula Double -> Formula Double -> Formula Double
    FSum :: CR -> CR -> Formula Double
    FSub :: CR -> CR -> Formula Double
    FSumL :: Char -> CR -> CR -> Formula Int
    FSumIf :: CR -> CR -> (Int -> Int) -> (Int -> Int) -> (Double -> Bool) -> Formula Double

instance Show (Formula a) where
    show _ = "$formula$"

feval :: Sheet -> Formula a -> a
feval _ (Frf cr) = cr
feval _ (Fnum n) = n
feval s (FDiv a b) = feval s a / feval s b
feval s (FAdd a b) = feval s a + feval s b
feval s (FSum a b) = (sum . map (unRef s)) (crRange a b)
feval s (FSub a b) = unRef s a - unRef s b

feval s (FSumL c a b) = (fromIntegral . length . filter (== c) . concatMap (unRefS s)) (crRange a b)

feval s (FSumIf a b ia ib t) = (sum . map (unRef s) . filter f) (crRange a b)
    where f (CR a' b') = t $ unRef s (CR (ia a') (ib b'))

-- * Cells

data Header

data Cell a where
    CellEmpty   :: Cell ()
    CellHeader  :: String    -> Cell Header
    CellString  :: String    -> Cell String
    CellInteger :: Integer   -> Cell Integer
    CellDouble  :: Double    -> Cell Double
    CellFormula :: Formula Double -> Cell (Formula Double)
    CellFormulaInt :: Formula Int -> Cell (Formula Int)
    CellJoin    :: [Cell'] -> Cell [()]

instance Show (Cell a) where
    show CellEmpty       = printf "%17s" ("" :: String)
    show (CellHeader  h) = printf "%17s" $ "= " ++ h ++ " ="
    show (CellString  s) = printf "%17s" s
    show (CellInteger i) = printf "%17s" (show i)
    show (CellDouble  d) = printf "%17f" d
    show (CellFormula f) = printf "%17s" (show f)
    show (CellFormulaInt f) = printf "%17s" (show f)
    show (CellJoin xs) = concatMap show xs

-- | wrapper
data Cell' = forall a. Cell' (Cell a)
instance Show Cell' where
    show (Cell' c) = show c

instance IsString Cell' where
    fromString (':':':':' ':s) = Cell' $ CellHeader s
    fromString s               = Cell' $ CellString s

class IsCell cell where
    cappend :: Col -> cell -> Col

instance IsCell (Cell c) where
    cappend ColEmpty   c  = Col c ColEmpty
    cappend (Col c cs) c' = Col c (cappend cs c')

instance IsCell Cell' where
    cappend col (Cell' c) = cappend col c

-- | Evalation of the cells depend on the sheet
showCellEval :: Sheet -> Cell' -> String
showCellEval sheet (Cell' (CellJoin xs)) = printf "%17s" (concatMap (showCellEval' sheet) xs)
showCellEval sheet (Cell' (CellFormula f)) = printf "%17.2f" $ feval sheet f
showCellEval sheet (Cell' (CellFormulaInt f)) = printf "%17d" $ feval sheet f
showCellEval _ c = show c

showCellEval' :: Sheet -> Cell' -> String
showCellEval' sheet (Cell' (CellJoin xs))      = concatMap (showCellEval' sheet) xs
showCellEval' _     (Cell' (CellString s))     = s
showCellEval' sheet (Cell' (CellFormula f))    = printf "%.2f" $ feval sheet f
showCellEval' sheet (Cell' (CellFormulaInt f)) = printf "%d" $ feval sheet f
showCellEval' _ c = show c

cellInteger :: Integer -> Cell'
cellInteger = Cell' . CellInteger

cellDouble :: Double -> Cell'
cellDouble = Cell' . CellDouble

cellFormula :: Formula Double -> Cell'
cellFormula = Cell' . CellFormula

cellFormulaInt :: Formula Int -> Cell'
cellFormulaInt = Cell' . CellFormulaInt

-- * Columns

data Col = ColEmpty
         | forall a. Col (Cell a) Col

instance Show Col where
    show ColEmpty   = ""
    show (Col c cs) = show c ++ show cs

(><) :: IsCell c => Col -> c -> Col
(><) = cappend

-- | construct a column with header
hcol :: Cell' -> [Cell'] -> Col
hcol (Cell' hc)             [] = Col hc ColEmpty
hcol (Cell' hc) (Cell' c : cs) = Col hc $ foldl
    (\col (Cell' c') -> col `cappend` c') (Col c ColEmpty) cs

colInsAt :: Int -> Cell' -> Col -> Col
colInsAt 0 (Cell' c)  ColEmpty  = Col c ColEmpty
colInsAt 0 (Cell' c) (Col _ cs) = Col c cs
colInsAt n cell       ColEmpty  = Col CellEmpty (colInsAt (n-1) cell ColEmpty)
colInsAt n cell      (Col c cs) = Col c         (colInsAt (n-1) cell cs)

colAt :: Int -> Sheet -> Col
colAt n (Sheet cs) = cs !! n

colAlterAt :: Int -> Sheet -> (Col -> Col) -> Sheet
colAlterAt n (Sheet cs) f = Sheet (take n cs ++ [f $ cs !! n] ++ drop (n+1) cs)

cellAt :: Int -> Col -> Cell'
cellAt 0 (Col c   _) = Cell' c
cellAt n (Col _ col) = cellAt (n-1) col
cellAt _ ColEmpty    = Cell' CellEmpty

colLength :: Col -> Int
colLength = colLength' 0
    where colLength' n ColEmpty = n
          colLength' n (Col _ cs) = colLength' (n + 1) cs

colSingle :: Cell' -> Col
colSingle (Cell' c) = Col c ColEmpty
