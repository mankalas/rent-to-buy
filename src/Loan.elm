module Loan exposing (Model, amortBegBalance, amortEndBalance, amortInterest, amortPrincipal, cost, interest, payment, paymentsNumber)


type alias Model =
    { term : Int -- Weeks
    , interest_rate : Float -- [0..1] p.a.
    , amount : Float -- $
    }


wInY : Float
wInY =
    52


w_interest_rate : Model -> Float
w_interest_rate loan =
    loan.interest_rate / wInY


w_interest : Model -> Float
w_interest loan =
    w_interest_rate loan * loan.amount


paymentsNumber : Model -> Int
paymentsNumber loan =
    loan.term


payment : Model -> Float
payment loan =
    let
        n =
            paymentsNumber loan |> toFloat

        r =
            w_interest_rate loan

        d =
            r / (1 - ((1 + r) ^ -n))
    in
    loan.amount * d


w_principal : Model -> Float -> Float
w_principal loan pay =
    pay - w_interest loan


interest_ : Model -> Float -> Float
interest_ loan pay =
    let
        wi =
            w_interest loan

        wp =
            w_principal loan pay

        new_loan =
            { loan
                | term = loan.term - 1
                , amount = loan.amount - wp
            }
    in
    if loan.term == 0 then
        0

    else
        wi + interest_ new_loan pay


interest : Model -> Float
interest loan =
    interest_ loan (payment loan)


afterPay : Model -> Float -> Model
afterPay loan pay =
    let
        wp =
            w_principal loan pay
    in
    { loan
        | term = loan.term - 1
        , amount = loan.amount - wp
    }


cost : Model -> Float
cost loan =
    interest loan + loan.amount


atNthIncrement : Model -> Int -> Float -> Model
atNthIncrement loan n pay =
    if n == 1 then
        loan

    else
        atNthIncrement (afterPay loan pay) (n - 1) pay


amortBegBalance : Model -> Int -> Float
amortBegBalance loan n =
    let
        new_loan =
            atNthIncrement loan n (payment loan)
    in
    new_loan.amount


amortEndBalance : Model -> Int -> Float
amortEndBalance loan n =
    amortBegBalance loan (n + 1)


amortInterest : Model -> Int -> Float
amortInterest loan n =
    let
        new_loan =
            atNthIncrement loan n (payment loan)
    in
    new_loan.interest_rate / wInY * new_loan.amount


amortPrincipal : Model -> Int -> Float
amortPrincipal loan n =
    let
        pay =
            payment loan

        new_loan =
            atNthIncrement loan n pay
    in
    w_principal new_loan pay
