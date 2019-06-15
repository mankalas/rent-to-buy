module LoanTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance, equal, within)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (wPayments)
import Test exposing (..)


suite : Test
suite =
    describe "The Loan module"
        [ describe "Weekly payment"
            [ test
                "Correct value"
              <|
                \_ ->
                    let
                        p =
                            wPayments 20 5 500100
                    in
                    p |> Expect.within (Expect.Relative 10) 753.4
            ]
        ]
