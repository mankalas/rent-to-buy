module LoanTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance, equal, within)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (interests, wInterest, wPayments)
import Test exposing (..)


suite : Test
suite =
    describe "The Loan 'module'"
        [ describe "Weekly payment"
            [ test "Correct value" <|
                \_ ->
                    let
                        p =
                            wPayments 20 5 500000
                    in
                    p |> Expect.within (Expect.Absolute 1) 760.78
            ]
        , describe "Weekly interest"
            [ test "Correct value" <|
                \_ ->
                    let
                        wi =
                            wInterest 5 500000
                    in
                    wi |> Expect.within (Expect.Relative 0.01) 480.77
            ]
        , describe "Total interest"
            [ test "Correct value" <|
                \_ ->
                    let
                        total =
                            interests (20 * 52) 5 500000 760.78
                    in
                    total |> Expect.within (Expect.Absolute 2) 291209.64
            ]
        ]
