hcalc
=====

hcalc is a spreadsheet toolkit for Haskell.

Formula DSL
-----------

hcalc contains a DSL for cell formulas similar to spreadsheet programs.
Here is some grammar (in some cases whitespace can be added or omitted):

    expr      ::= d | simple_calc
    d         ::= function | parens | - d | cell_ref | constant
    function  ::= (fname params)
    params    ::= param params | param
    param     ::= d | cell_rang
    parens    ::= (expr)
    simple_calc::= d op d
    op        ::= + | - | * | /
    cell_rang ::= cell_ref:cell_ref
    cell_ref  ::= <col><row>
    col       ::= <capital letters>
    row       ::= <digits>
    constant  ::= <digits>[.<digits>]


Example outputs
---------------

from `opsu.hs`:

    ----- opsu 1/4 -----

           = kurssi = |  =  = | = op = | = x = |  = hyväksiluvut = | = syksy 2013 = | = kevät 2014 =
            Työväline |       |      1 |     1 |                   |              x |              x
        TVT-ajokortti |       |      3 |     1 |                   |              x |
                 JTKT |       |      6 |     1 |                 x |                |
             Ohj. Pe. |       |      5 |     1 |                 x |                |
           Ohj. jatk. |       |      4 |     1 |                 x |                |
           Ohj. harj. |       |      4 |     1 |                 x |                |
          Ohj. menet. |       |      4 |     1 |                 x |                |
          Tiet. toim. |       |      4 |     1 |                 x |                |
                 TiPe |       |      4 |     1 |                   |              x |
                  JYM |       |     10 |     1 |                 x |                |
                 Ana1 |       |      5 |     1 |                   |              x |
                 Lin1 |       |      5 |     1 |                   |              x |
         Lask. Mallit |       |      6 |     1 |                   |                |              x
                  ... |       |        |     0 |                   |                |
           TOTAL (op) |    50 |        |       |                   |                |


    ----- opsu 2/4 -----

           = kurssi = |  =  = | = op = | = x = |  = hyväksiluvut = | = syksy 2013 = | = kevät 2014 =
            Työväline |       |      1 |     2 |                   |              x |              x
        TVT-ajokortti |       |      3 |     1 |                   |              x |
                 JTKT |       |      6 |     1 |                 x |                |
             Ohj. Pe. |       |      5 |     1 |                 x |                |
           Ohj. jatk. |       |      4 |     1 |                 x |                |
           Ohj. harj. |       |      4 |     1 |                 x |                |
          Ohj. menet. |       |      4 |     1 |                 x |                |
          Tiet. toim. |       |      4 |     1 |                 x |                |
                 TiPe |       |      4 |     1 |                   |              x |
                  JYM |       |     10 |     1 |                 x |                |
                 Ana1 |       |      5 |     1 |                   |              x |
                 Lin1 |       |      5 |     1 |                   |              x |
         Lask. Mallit |       |      6 |     1 |                   |                |              x
                  ... |       |        |     0 |                   |                |
           TOTAL (op) |    50 |        |       |                   |                |


    ----- opsu 3/4 -----

           = kurssi = |  =  = | = op = | = x = |  = hyväksiluvut = | = syksy 2013 = | = kevät 2014 =
            Työväline |       |      1 |     2 |                   |              x |              x
        TVT-ajokortti |       |      3 |     1 |                   |              x |
                 JTKT |       |      6 |     1 |                 x |                |
             Ohj. Pe. |       |      5 |     1 |                 x |                |
           Ohj. jatk. |       |      4 |     1 |                 x |                |
           Ohj. harj. |       |      4 |     1 |                 x |                |
          Ohj. menet. |       |      4 |     1 |                 x |                |
          Tiet. toim. |       |      4 |     1 |                 x |                |
                 TiPe |       |      4 |     1 |                   |              x |
                  JYM |       |     10 |     1 |                 x |                |
                 Ana1 |       |      5 |     1 |                   |              x |
                 Lin1 |       |      5 |     1 |                   |              x |
         Lask. Mallit |       |      6 |     1 |                   |                |              x
                  ... |       |        |     0 |                   |                |
           TOTAL (op) | 61.00 | /61.00 | 14.00 |                 7 |              5 |              2


    ----- opsu 4/4 -----

           = kurssi = |  =  = | = op = | = x = |  = hyväksiluvut = | = syksy 2013 = | = kevät 2014 =
            Työväline |       |      1 |     2 |                   |              x |              x
        TVT-ajokortti |       |      3 |     1 |                   |              x |
                 JTKT |       |      6 |     1 |                 x |                |
             Ohj. Pe. |       |      5 |     1 |                 x |                |
           Ohj. jatk. |       |      4 |     1 |                 x |                |
           Ohj. harj. |       |      4 |     1 |                 x |                |
          Ohj. menet. |       |      4 |     1 |                 x |                |
          Tiet. toim. |       |      4 |     1 |                 x |                |
                 TiPe |       |      4 |     1 |                   |              x |
                  JYM |       |     10 |     1 |                 x |                |
                 Ana1 |       |      5 |     1 |                   |              x |
                 Lin1 |       |      5 |     1 |                   |              x |
         Lask. Mallit |       |      6 |     1 |                   |                |              x
                  ... |       |        |     0 |                   |                |
           TOTAL (op) | 61.00 | /61.00 | 14.00 |             37.00 |          18.00 |           7.00


from `verirahat.hs`:

    ----- verirahat 1/4 -----

            = What? = |   = Matti-Sampe =
             ab-lippu |               2.1
        Wombats kalja |              -2.8
                Sushi |               9.0
          8mm tequila |              -2.0
            Yesterday |               2.8
                 Taxi |              -4.5
            Aamiainen |              11.5
      Pumpattava octo |              -7.5
         Sampe maksaa |              4.30


    ----- verirahat 2,3/4 -----

            = What? = | = Matti = | = Sampe = | = Joku = |  = Muu = |
             ab-lippu |       2.0 |           |          |          |
        Wombats kalja |           |       2.0 |          |          |
                Sushi |           |           |          |          |
          8mm tequila |           |           |          |          |
            Yesterday |           |           |          |          |
                 Taxi |           |           |          |          |
            Aamiainen |           |           |          |          |
      Pumpattava octo |           |           |          |          |
              = TOT = |      2.00 |      2.00 |     0.00 |     0.00 | 4.00
                   /4 |           |           |          |          | 1.00


    ----- verirahat 4/4 -----

            = What? = | = Matti = | = Sampe = | = Joku = |  = Muu = |
             ab-lippu |       2.0 |           |          |          |
        Wombats kalja |           |       2.0 |          |          |
                Sushi |           |           |          |          |
          8mm tequila |           |           |          |          |
            Yesterday |           |           |          |          |
                 Taxi |           |           |          |          |
            Aamiainen |           |           |          |          |
      Pumpattava octo |           |           |          |          |
              = TOT = |      2.00 |      2.00 |     0.00 |     0.00 | 4.00
                   /4 |           |           |          |          | 1.00
          maksettavaa |     -1.00 |     -1.00 |     1.00 |     1.00 |


