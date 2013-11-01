{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Hcalc
import Fdsl
default (Cell')

t1 :: Sheet
t1 = Sheet
    [ hcol ":: kurssi"
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
    , colInsAt 15 (cellInteger 50) $ hcol ":: " []
    , hcol ":: op"
        (map cellInteger [ 1, 3, 6, 5, 4, 4, 4, 4, 4, 10, 5, 5, 6 ])

    , hcol ":: x" (replicate 13 (cellInteger 1) ++ [cellInteger 0] )

    , hcol ":: hyväksiluvut"
        (  replicate 2 (Cell' CellEmpty)
        ++ replicate 6 "x"
        ++ [Cell' CellEmpty] ++ ["x"]
        )
    , colInsAt 12 "x" $ colInsAt 11 "x" $ colInsAt 9 "x"
        $ hcol ":: syksy 2013" [ "x", "x" ]
    , colInsAt 13 "x" $ hcol ":: kevät 2014"
        [ "x" ]
    ]

t2 :: Sheet
t2 = colAlterAt 3 t1 $ const $ hcol ":: x"
    $ map (\n -> cellFormulaInt $ FSumL 'x' (CR 4 n) (CR 6 n)) [1..14] 

t3 :: Sheet
t3 = let 
    a1 = colAlterAt 2 t2 $ colInsAt 15 (Cell' $ CellJoin ["/", cellFormula $ formula "(sum B1:B14)"]) -- (FSum (CR 2 1) (CR 2 14))])
    a2 = colAlterAt 3 a1 $ colInsAt 15 (cellFormula $ formula "(sum C1:C14) + 5") -- (FSum (CR 3 1) (CR 3 14)))
    a3 = colAlterAt 4 a2 $ colInsAt 15 (cellFormulaInt (FSumL 'x' (CR 4 1) (CR 4 14)))
    a4 = colAlterAt 5 a3 $ colInsAt 15 (cellFormulaInt (FSumL 'x' (CR 5 1) (CR 5 14)))
    a5 = colAlterAt 6 a4 $ colInsAt 15 (cellFormulaInt (FSumL 'x' (CR 6 1) (CR 6 14)))
    a6 = colAlterAt 1 a5 $ colInsAt 15
          (cellFormula (FSumIf (CR 2 1) (CR 2 14) (+1) id (> 0) ))
    in a6

t4 :: Sheet
t4 = eachCol t3 f
    where f n col
            | n >= 4    = colInsAt 15 (cellFormula (FSumIf (CR 2 1) (CR 2 14) (+(n-2)) id (> 0))) col
            | otherwise = col

main :: IO ()
main = do
    printWithTitle t1 "opsu 1/4"
    printWithTitle t2 "opsu 2/4"
    printWithTitle t3 "opsu 3/4"
    printWithTitle t4 "opsu 4/4"
