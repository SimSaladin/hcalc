{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hcalc.Examples where

import Hcalc

t1 :: Sheet
t1 = sheet
    [ column ":: kurssi"
        [ "Työväline"
        , "TVT-ajokortti"
        , "JTKT"
        , "Ohj. Pe."
        , "Ohj. jatk." --
        , "Ohj. harj."
        , "Ohj. menet."
        , "Tiet. toim."
        , "TiPe"
        , "JYM"
        , "Ana1"
        , "Lin1"
        , "Lask. Mallit"
        , "..."
        , "TOTAL (op)"
        ]
    , column ":: "              []          & insert 15 (int 50)
    , column ":: op"            (map int [ 1, 3, 6, 5, 4, 4, 4, 4, 4, 10, 5, 5, 6 ])
    , column ":: x"             (replicate 13 (int 1) ++ [int 0])
    , column ":: hyväksiluvut"  (replicate 2 empty ++ replicate 6 "x" ++ [empty] ++ ["x"])
    , column ":: syksy 2013"    ["x","x"]   & insert 9 "x" & insert 11 "x" & insert 12 "x"
    , column ":: kevät 2014"    ["x"]       & insert 13 "x"
    ]

t2 :: Sheet
t2 = t1 & setCol 3 myColumn
    where
        myColumn = column ":: x" $ map (\n -> formulaInt' $ FSumL 'x' (4,n) (6,n)) [1..14] 

t3 :: Sheet
t3 = t2
    & onCol 2 (insert 15 $ combine ["/", formula "(sum B1:B14)"])
    & onCol 3 (insert 15 $ formula "(sum C1:C14)")
    & onCol 4 (insert 15 $ formulaInt' (FSumL 'x' (4, 1) (4, 14)))
    & onCol 5 (insert 15 $ formulaInt' (FSumL 'x' (5, 1) (5, 14)))
    & onCol 6 (insert 15 $ formulaInt' (FSumL 'x' (6, 1) (6, 14)))
    & onCol 1 (insert 15 $ formula' (FSumIf (2, 1) (2, 14) (+1) id (> 0) ))

t4 :: Sheet
t4 = mapCols t3 f
    where f n col
            | n >= 4    = insert 15 (formula' (FSumIf (2, 1) (2, 14) ((n-2)+) id (> 0))) col
            | otherwise = col

main :: IO ()
main = do
    printWithTitle t1 "opsu 1/4"
    printWithTitle t2 "opsu 2/4"
    printWithTitle t3 "opsu 3/4"
    printWithTitle t4 "opsu 4/4"
