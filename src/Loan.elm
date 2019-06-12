module Loan exposing (Model, updateAmount, updateRate, updateTerm)

import Utils exposing (updateFloat, updateInt)


type alias Model =
    { amount : Float
    , term : Int
    , ratePerAnnum : Float
    }


type alias InterestRate =
    { ratePerAnnum : Float
    }


updateAmount : String -> Model -> Result String Model
updateAmount a m =
    updateFloat a m "Bad loan amount" (\x -> Model x m.term m.ratePerAnnum)


updateTerm : String -> Model -> Result String Model
updateTerm t m =
    updateInt t m "Bad loan term" (\x -> Model m.amount x m.ratePerAnnum)


updateRate : String -> Model -> Result String Model
updateRate r m =
    updateFloat r m "Bad loan rate" (\x -> Model m.amount m.term x)
