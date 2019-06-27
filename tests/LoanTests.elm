module LoanTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance, equal, within)
import Fuzz exposing (Fuzzer, int, list, string)
import Loan exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        loan =
            Loan.Model (52 * 20 + 4) 0.05 500000 Loan.Yearly
    in
    describe "The Loan module"
        [ describe "Payment"
            [ test "Correct weekly value" <|
                \_ ->
                    Loan.payment loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 745.97
            , test "Correct monthly value" <|
                \_ ->
                    Loan.payment loan Loan.Monthly
                        |> Expect.within (Expect.Absolute 1) 3237.59
            , test "Correct yearly value" <|
                \_ ->
                    Loan.payment loan Loan.Yearly
                        |> Expect.within (Expect.Absolute 1) 39733.6
            ]
        , describe "Total number of payments"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.paymentsNumber loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 1057.33
            , test "Monthly payments" <|
                \_ ->
                    Loan.paymentsNumber loan Loan.Monthly
                        |> Expect.within (Expect.Absolute 1) 244
            , test "Yearly payments" <|
                \_ ->
                    Loan.paymentsNumber loan Loan.Yearly
                        |> Expect.within (Expect.Absolute 1) 20.33
            ]
        , describe "Total interest"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.interest loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 288737.53
            , test "Monthly payments" <|
                \_ ->
                    Loan.interest loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 289972.43
            , test "Yearly payments" <|
                \_ ->
                    Loan.interest loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 307916.49
            ]
        , describe "Total cost"
            [ test "Weekly payments" <|
                \_ ->
                    Loan.cost loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 788737.53
            , test "Monthly payments" <|
                \_ ->
                    Loan.cost loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 789972.43
            , test "Yearly payments" <|
                \_ ->
                    Loan.cost loan Loan.Weekly
                        |> Expect.within (Expect.Absolute 1) 807916.49
            ]
        , describe "Amortisation table"
            [ describe "Beginning balance"
                [ describe "First increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Weekly 1
                                |> Expect.within (Expect.Absolute 1) 500000
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Monthly 1
                                |> Expect.within (Expect.Absolute 1) 500000
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Yearly 1
                                |> Expect.within (Expect.Absolute 1) 500000
                    ]
                , describe "Halfway"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Weekly 529
                                |> Expect.within (Expect.Absolute 1) 311065.95
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Monthly 122
                                |> Expect.within (Expect.Absolute 1) 312726.85
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Yearly 10
                                |> Expect.within (Expect.Absolute 1) 337539.04
                    ]
                , describe "Last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Weekly 1058
                                |> Expect.within (Expect.Absolute 1) 248.5
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Monthly 244
                                |> Expect.within (Expect.Absolute 1) 3224.46
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortBegBalance loan Loan.Yearly 21
                                |> Expect.within (Expect.Absolute 1) 12819.53
                    ]
                ]
            , describe "Ending balance"
                [ describe "First increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Weekly 1
                                |> Expect.within (Expect.Absolute 1) 499723.39
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Monthly 1
                                |> Expect.within (Expect.Absolute 1) 500000
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Yearly 1
                                |> Expect.within (Expect.Absolute 1) 500000
                    ]
                , describe "Halfway"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Weekly 529
                                |> Expect.within (Expect.Absolute 1) 310611.98
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Monthly 122
                                |> Expect.within (Expect.Absolute 1) 312726.85
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Yearly 10
                                |> Expect.within (Expect.Absolute 1) 337539.04
                    ]
                , describe "Before last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Weekly 1057
                                |> Expect.within (Expect.Absolute 1) 248.5
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Monthly 244
                                |> Expect.within (Expect.Absolute 1) 3224.46
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Yearly 21
                                |> Expect.within (Expect.Absolute 1) 12819.53
                    ]
                , describe "Last increment"
                    [ test "Weekly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Weekly 1058
                                |> Expect.within (Expect.Absolute 1) 0
                    , test "Monthly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Monthly 244
                                |> Expect.within (Expect.Absolute 1) 0
                    , test "Yearly payments" <|
                        \_ ->
                            Loan.amortEndBalance loan Loan.Yearly 21
                                |> Expect.within (Expect.Absolute 1) 0
                    ]
                ]
            ]
        , describe "Interest"
            [ describe "First increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Weekly 1
                            |> Expect.within (Expect.Absolute 1) 469.36
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Monthly 1
                            |> Expect.within (Expect.Absolute 1) 500000
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Yearly 1
                            |> Expect.within (Expect.Absolute 1) 500000
                ]
            , describe "Halfway"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Weekly 529
                            |> Expect.within (Expect.Absolute 1) 292
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Monthly 122
                            |> Expect.within (Expect.Absolute 1) 312726.85
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Yearly 10
                            |> Expect.within (Expect.Absolute 1) 337539.04
                ]
            , describe "Last increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Weekly 1058
                            |> Expect.within (Expect.Absolute 1) 0.23
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Monthly 244
                            |> Expect.within (Expect.Absolute 1) 3224.46
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortInterest loan Loan.Yearly 21
                            |> Expect.within (Expect.Absolute 1) 12819.53
                ]
            ]
        , describe "Principal"
            [ describe "First increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Weekly 1
                            |> Expect.within (Expect.Absolute 1) 3
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Monthly 1
                            |> Expect.within (Expect.Absolute 1) 500000
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Yearly 1
                            |> Expect.within (Expect.Absolute 1) 500000
                ]
            , describe "Halfway"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Weekly 529
                            |> Expect.within (Expect.Absolute 1) 453.97
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Monthly 122
                            |> Expect.within (Expect.Absolute 1) 312726.85
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Yearly 10
                            |> Expect.within (Expect.Absolute 1) 337539.04
                ]
            , describe "Last increment"
                [ test "Weekly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Weekly 1058
                            |> Expect.within (Expect.Absolute 1) 248.5
                , test "Monthly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Monthly 244
                            |> Expect.within (Expect.Absolute 1) 3224.46
                , test "Yearly payments" <|
                    \_ ->
                        Loan.amortPrincipal loan Loan.Yearly 21
                            |> Expect.within (Expect.Absolute 1) 12819.53
                ]
            ]
        ]
