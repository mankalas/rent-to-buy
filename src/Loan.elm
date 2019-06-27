module Loan exposing (Frequency(..), Model, interests, interestsAt, total, wInM, wInY, wInterest, wPayments, wRate)


type Frequency
    = Weekly
    | Monthly
    | Yearly


type alias Model =
    { term : Int -- Months
    , interest_rate : Float -- [0..1]
    , amount : Float -- $
    , coumpound : Frequency
    , pay_back : Frequency
    }


wInY =
    52


wInM =
    4.348214


wPayments : Model -> Float
wPayments loan =
    let
        n =
            toFloat loan.term * wInY

        i =
            loan.interest_rate / toFloat 100 / wInY

        d =
            (((1 + i) ^ n) - 1) / (i * (1 + i) ^ n)
    in
    loan.amount / d


wRate lr =
    lr / toFloat 100 / wInY


wInterest lr la =
    wRate lr * la


interestsAt : Model -> Float -> Int -> Float
interestsAt loan pay n =
    let
        weekly_rate =
            wRate loan.interest_rate

        weekly_interest =
            weekly_rate * loan.amount

        principal_pay =
            pay - weekly_interest
    in
    if loan.term == n || loan.term <= 0 then
        0

    else
        let
            new_loan =
                { loan | term = loan.term - 1, amount = loan.amount - principal_pay }
        in
        weekly_interest + interestsAt new_loan pay n


interests : Model -> Float
interests loan =
    let
        w_pay =
            wPayments loan
    in
    interestsAt loan w_pay 0


total : Model -> Float
total loan =
    loan.amount + interests loan
