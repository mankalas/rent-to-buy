module Loan exposing (interests, interestsAt, wInM, wInY, wInterest, wPayments, wRate)


wInY =
    52


wInM =
    4.348214


wPayments : Int -> Float -> Float -> Float
wPayments lt lr la =
    let
        n =
            toFloat lt * wInY

        i =
            lr / toFloat 100 / wInY

        d =
            (((1 + i) ^ n) - 1) / (i * (1 + i) ^ n)
    in
    la / d


wRate lr =
    lr / toFloat 100 / wInY


wInterest lr la =
    wRate lr * la


interestsAt : Int -> Float -> Float -> Float -> Int -> Float
interestsAt lt lr la pay n =
    let
        weekly_rate =
            wRate lr

        weekly_interest =
            weekly_rate * la

        principal_pay =
            pay - weekly_interest
    in
    if lt == n then
        0

    else
        weekly_interest + interestsAt (lt - 1) lr (la - principal_pay) pay n


interests : Int -> Float -> Float -> Float -> Float
interests lt lr la pay =
    interestsAt lt lr la pay 0
