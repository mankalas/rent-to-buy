module LoanTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance, equal, within)
import Fuzz exposing (Fuzzer, int, list, string)
import Loan exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        loan =
            Loan.Model (52 * 20) 0.05 500000
    in
    describe "The Loan module"
        [ describe "Total number of payments"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.paymentsNumber loan
                        |> Expect.equal 1040
            ]
        , describe "Payment"
            [ test "Correct weekly value" <|
                \_ ->
                    Loan.payment loan
                        |> Expect.within (Expect.Absolute 1) 760.78
            ]
        , describe "Total interest"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.interest loan
                        |> Expect.within (Expect.Absolute 1.5) 291208.6
            ]
        , describe "Total cost"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.cost loan
                        |> Expect.within (Expect.Absolute 1.5) 791208.6
            ]
        , describe "Amortisation table"
            [ describe "Beginning balance"
                [ describe "First increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan 1
                                |> Expect.within (Expect.Absolute 0.01) 500000
                    ]
                , describe "Halfway"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan 529
                                |> Expect.within (Expect.Absolute 1) 307495.7
                    ]
                , describe "Last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan 1040
                                |> Expect.within (Expect.Absolute 1) 760
                    ]
                ]
            , describe "Ending balance"
                [ describe "First increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan 1
                                |> Expect.within (Expect.Absolute 0.01) 499719.99
                    ]
                , describe "Halfway"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan 529
                                |> Expect.within (Expect.Absolute 1) 307030.59
                    ]
                , describe "Before last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan 1039
                                |> Expect.within (Expect.Absolute 1) 760
                    ]
                , describe "Last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan 1040
                                |> Expect.within (Expect.Absolute 1) 0
                    ]
                ]
            ]
        , describe "Interest"
            [ describe "First increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan 1
                            |> Expect.within (Expect.Absolute 1) 480.77
                ]
            , describe "Halfway"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan 529
                            |> Expect.within (Expect.Absolute 1) 295.67
                ]
            , describe "Last increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan 1040
                            |> Expect.within (Expect.Absolute 1) 0.73
                ]
            ]
        , describe "Principal"
            [ describe "First increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan 1
                            |> Expect.within (Expect.Absolute 1) 280.01
                ]
            , describe "Halfway"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan 529
                            |> Expect.within (Expect.Absolute 1) 465.11
                ]
            , describe "Last increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan 1040
                            |> Expect.within (Expect.Absolute 1) 760
                ]
            ]
        ]
