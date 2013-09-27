{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Hcalc
default (Cell')

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
   , hcol ":: Matti-Sampe" $
       map cellDouble [  2.1
                      , -2.8
                      ,  9
                      , -2
                      ,  2.8
                      , -4.5
                      , 11.5
                      , -7.5
                      ]
       ++ [cellFormula (FDiv (FSum (CR 1 1) (CR 1 8)) (Fnum 2))]
   ]

t2 :: Sheet
t2 = Sheet
    [ colInsAt 9 ":: TOT" (colAt 0 t1) >< "/4"

    , f 1 $ hcol ":: Matti" [                  cellDouble 2 ]
    , f 2 $ hcol ":: Sampe" [ Cell' CellEmpty, cellDouble 2 ]
    , f 3 $ hcol ":: Joku"  []
    , f 4 $ hcol ":: Muu"   []

    , colInsAt 9 (cellFormula (FSum (CR 1 9) (CR 4 9)) ) ColEmpty
        >< cellFormula (FDiv  (FSum (CR 1 9) (CR 4 9)) (Fnum 4))
    ]

t4 :: Sheet
t4 = eachCol t2 g where
    g 0 col = col >< "maksettavaa"
    g 5 col = col
    g n col = colInsAt 11 (cellFormula (FSub (CR 5 10) (CR n 9))) col


-- * ui

-- | subst. col 9 with a sum
f :: Int -> Col -> Col
f n = colInsAt 9 (cellFormula $ FSum (CR n 1) (CR n 8))

main :: IO ()
main = do
    printWithTitle t1 "verirahat 1/4"
    printWithTitle t2 "verirahat 2,3/4"
    printWithTitle t4 "verirahat 4/4"
