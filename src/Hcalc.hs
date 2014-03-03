{-# LANGUAGE GADTs, FlexibleInstances, OverloadedStrings,
    ExistentialQuantification, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Spreadsheet functionality in haskell
module Hcalc
    ( Sheet, Col, Cell

    -- * Construct
    , (&)
    , sheet
    -- ** Columns
    , column
    , single
    -- *** Modify
    , insert
    , onCol
    , setCol
    , mapCols

    -- ** Cells
    , empty
    , int
    , double
    , combine
    , formula'
    , formulaInt'

    -- * Render
    , printWithTitle
    , evalSheet
    , showSheet

    -- * Formula
    , feval
    , formula
    , Formula(..)

    -- * Import
    -- * Export
    , 
    ) where

import Hcalc.Internal
import Hcalc.Formula

-- Huh?
colNames :: String
colNames = ['A' .. 'Z']

(&) :: a -> (a -> b) -> b
(&) = flip ($)

-- * Columns

-- | construct a column with header
column :: String -> [Cell'] -> Col
column header cs = foldl (><) ColEmpty cs >< CellHeader header

-- | "colInsAt n c cs" Insert cell c into column at pos n in cs.
insert :: Int -> Cell' -> Col -> Col
insert 0 cell ColEmpty   = Col cell ColEmpty
insert 0 cell (Col _ cs) = Col cell cs
insert n cell ColEmpty   = Col (Cell' CellEmpty) (insert (n-1) cell ColEmpty)
insert n cell (Col c cs) = Col c                 (insert (n-1) cell cs)

single :: ToCell c => c -> Col
single c = Col (toCell c) ColEmpty

-- | Append cell
(><) :: ToCell c => Col -> c -> Col
col >< c = Col (toCell c) col

colAt :: Int -> Sheet -> Col
colAt n (Sheet cs) = cs !! n

-- | Apply a transformation in given col
onCol :: Int -> (Col -> Col) -> Sheet -> Sheet
onCol n f (Sheet cs) = Sheet (take n cs ++ [f $ cs !! n] ++ drop (n+1) cs)

-- | Execute a transformation on each column
mapCols :: Sheet -> (Int -> Col -> Col) -> Sheet
mapCols (Sheet xs) f = Sheet (zipWith f [0..] xs)

setCol :: Int -> Col -> Sheet -> Sheet
setCol n c = onCol n (const c)

-- * Cells

empty :: Cell'
empty = Cell' CellEmpty

int :: Integer -> Cell'
int = Cell' . CellInteger

double :: Double -> Cell'
double = Cell' . CellDouble

combine :: [Cell'] -> Cell'
combine = Cell'. CellJoin

formula :: String -> Cell'
formula = Cell' . CellFormula . formulaParse

formula' :: Formula Double -> Cell'
formula' = Cell' . CellFormula

formulaInt' :: Formula Int -> Cell'
formulaInt' = Cell' . CellFormulaInt
