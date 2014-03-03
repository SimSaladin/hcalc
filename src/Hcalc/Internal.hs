{-# LANGUAGE GADTs, FlexibleInstances, OverloadedStrings,
    ExistentialQuantification, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Internal module of hcalc (read: unorganized monster under the hood).
module Hcalc.Internal where

import Data.String
import Text.Printf
import Hcalc.Formula

-- * Sheets

-- | A spreadsheet is a list of columns
newtype Sheet = Sheet [Col]
--instance IsList Sheet -- :3 (with -XOverloadedLists, requires GHC HEAD or 7.8?)
instance Show Sheet where
    show = showSheet show

sheet :: [Col] -> Sheet
sheet = Sheet

feval :: Sheet -> Formula a -> a
feval _ (Fnum n) = n
feval s (FnumCR cr) = unRef s cr
feval s (FMul a b) = feval s a * feval s b
feval s (FDiv a b) = feval s a / feval s b
feval s (FAdd a b) = feval s a + feval s b
feval s (FSub a b) = feval s a - feval s b
feval s (FNeg a)   = - feval s a
feval s (FSum (a, b)) = (sum . map (unRef s)) (crRange a b)
feval s (FSumL c a b) = (fromIntegral . length . filter (== c) . concatMap (unRefS s)) (crRange a b)
feval s (FSumIf a b ia ib t) = (sum . map (unRef s) . filter f) (crRange a b)
    where f (a', b') = t $ unRef s (ia a', ib b')

evalSheet :: Sheet -> String
evalSheet sh = showSheet (showCellEval sh) sh

-- | Render a Sheet as text where a function is applied to each cell.
showSheet :: (Cell' -> String) -> Sheet -> String
showSheet viewCell (Sheet (x:xs)) = foldl
    (\s n -> s ++ showRow n ++ "\n") ""
    [0 .. maximum (map colLength (x:xs)) - 1]
    where
        showRow n = foldl (\s c -> s ++ " | " ++ viewCell (cellAt n c))
                          (viewCell $ cellAt n x)
                          xs

-- | Print a sheet with a title
printWithTitle :: Sheet -> String -> IO ()
printWithTitle sh s = do
    putStrLn $ "\n----- " ++ s ++ " -----\n"
    putStrLn $ evalSheet sh

-- * Columns

data Col = ColEmpty
         | Col Cell' Col
instance Show Col where
    show ColEmpty   = ""
    show (Col c cs) = show c ++ show cs

colLength :: Col -> Int
colLength = colLength' 0
    where colLength' n ColEmpty = n
          colLength' n (Col _ cs) = colLength' (n + 1) cs

cellAt :: Int -> Col -> Cell'
cellAt 0 (Col c  _) = c
cellAt n (Col _ cs) = cellAt (n-1) cs
cellAt _ ColEmpty   = Cell' CellEmpty

-- * Cells

data Cell a where
    CellEmpty      :: Cell ()
    CellHeader     :: String         -> Cell Header
    CellString     :: String         -> Cell String
    CellInteger    :: Integer        -> Cell Integer
    CellDouble     :: Double         -> Cell Double
    CellFormula    :: Formula Double -> Cell (Formula Double)
    CellFormulaInt :: Formula Int    -> Cell (Formula Int)
    CellJoin       :: [Cell']        -> Cell [()]
instance ToCell (Cell c) where toCell = Cell'
instance ToCell Cell'    where toCell = id
instance Show (Cell a) where
    show CellEmpty       = ""
    show (CellHeader h)  = printf "%17s" ("= " ++ h ++ " =")
    show (CellString  s) = printf "%17s" s
    show (CellInteger i) = printf "%17s" (show i)
    show (CellDouble  d) = printf "%17f" d
    show (CellFormula f)    = printf "%17s" (show f)
    show (CellFormulaInt f) = printf "%17s" (show f)
    show (CellJoin xs) = concatMap show xs

-- | Existential wrapper for cells
data Cell' = forall a. Cell' (Cell a)
instance IsString Cell' where
    fromString (':':':':' ':s) = Cell' $ CellHeader s
    fromString s               = Cell' $ CellString s
instance Show Cell' where show (Cell' c) = show c

class ToCell c where
    toCell :: c -> Cell'

data Header

-- | Evalation of the cells depend on the sheet
showCellEval :: Sheet -> Cell' -> String
showCellEval sh (Cell' (CellJoin xs)) = printf "%17s" (concatMap (showCellEval' sh) xs)
showCellEval sh (Cell' (CellFormula f)) = printf "%17.2f" $ feval sh f
showCellEval sh (Cell' (CellFormulaInt f)) = printf "%17d" $ feval sh f
showCellEval _ c = show c

showCellEval' :: Sheet -> Cell' -> String
showCellEval' sh (Cell' (CellJoin xs))      = concatMap (showCellEval' sh) xs
showCellEval' _     (Cell' (CellString s))     = s
showCellEval' sh (Cell' (CellFormula f))    = printf "%.2f" $ feval sh f
showCellEval' sh (Cell' (CellFormulaInt f)) = printf "%d" $ feval sh f
showCellEval' _ c = show c

-- * Refs

-- | Rectangular selection
crRange :: CR -> CR -> [CR]
crRange (x, y) (x', y') = [ (a, b) | a <- [x..x'], b <- [y..y'] ]

-- | Dereference a CR
unRef :: Sheet -> CR -> Double
-- TODO return only a value
unRef sh@(Sheet cols) (c, r) = case cellAt r (cols !! c) of
    Cell' (CellDouble d)  -> d
    Cell' (CellInteger d) -> fromIntegral d
    Cell' (CellFormula f) -> feval sh f
    Cell' (CellFormulaInt f) -> fromIntegral $ feval sh f
    Cell' (CellString _)  -> 1 -- FIXME: hax to generalize FSumIf
    Cell' CellEmpty       -> 0
    _ -> error "unRef: only doubles, formula doubles and empties as 0s are supported as refs"

-- | Dereference a CR to string
unRefS :: Sheet -> CR -> String
unRefS (Sheet cols) (c, r) = case cellAt r (cols !! c) of
    Cell' (CellString s) -> s
    Cell' CellEmpty      -> ""
    _                    -> error "unRefS: only strings accepted in cells"

