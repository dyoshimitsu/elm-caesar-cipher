module CaesarSpec exposing (..)

import Caesar exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "The Caesar module"
        [ describe "Caesar.crack"
            [ test "crackable case 1" <|
                \_ ->
                    "kdvnhoo lv ixq"
                        |> Caesar.crack
                        |> Expect.equal "haskell is fun"
            , test "crackable case 2" <|
                \_ ->
                    "vscd mywzboroxcsyxc kbo ecopev"
                        |> Caesar.crack
                        |> Expect.equal "list comprehensions are useful"
            ]
        ]
