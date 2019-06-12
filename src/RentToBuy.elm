module RentToBuy exposing (main)

import Browser
import House as House exposing (Model, updateRate, updateValue)
import Html exposing (Html, button, div, h1, input, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Loan as Loan exposing (Model, updateAmount, updateRate, updateTerm)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Constraint
    = HouseC
    | PaymentC


type alias Deposit =
    { current : Float
    , wContribution : Float
    }


type alias Insurance =
    { yAmount : Float }


type alias Tax =
    { yAmount : Float }


type alias Contract =
    { amount : Float
    , term : Int
    }


type alias Model =
    { house : House.Model
    , deposit : Deposit
    , loan : Loan.Model
    , insurance : Insurance
    , tax : Tax
    , wPayment : Float
    , contract : Contract
    , constraint : Constraint
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (House.Model 500000.0 0.1)
        (Deposit 10000.0 50)
        (Loan.Model 500000 (12 * 20) 0.06)
        (Insurance 3000)
        (Tax 3000)
        650.0
        (Contract 500000.0 (12 * 3))
        HouseC
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = None
    | ChangeHouseRate String
    | ChangeHouseValue String
    | ChangeLoanAmount String
    | ChangeLoanRate String
    | ChangeLoanTerm String


updateRecord : Model -> Result String m -> (m -> Model) -> Model
updateRecord m rec contr =
    case rec of
        Err err ->
            { m | error = Just err }

        Ok r ->
            contr r


updateHouse : Model -> Result String House.Model -> Model
updateHouse m h =
    updateRecord m h (\r -> { m | house = r })


updateLoan : Model -> Result String Loan.Model -> Model
updateLoan m l =
    updateRecord m l (\r -> { m | loan = r })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        ChangeHouseRate s ->
            ( updateHouse model <| House.updateRate s model.house, Cmd.none )

        ChangeHouseValue s ->
            ( updateHouse model <| House.updateValue s model.house, Cmd.none )

        ChangeLoanAmount s ->
            ( updateLoan model <| Loan.updateAmount s model.loan, Cmd.none )

        ChangeLoanTerm s ->
            ( updateLoan model <| Loan.updateTerm s model.loan, Cmd.none )

        ChangeLoanRate s ->
            ( updateLoan model <| Loan.updateRate s model.loan, Cmd.none )



-- VIEW


viewLoan : Model -> Html Msg
viewLoan model =
    table []
        [ thead []
            [ tr [ colspan 2 ] [ text "Loan" ]
            , tr []
                [ th [] [ text "Amount" ]
                , th [] [ text "Rate" ]
                , th [] [ text "Duration (m.)" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ input
                        [ placeholder "Amount"
                        , value <| String.fromFloat model.loan.amount
                        , onInput ChangeLoanAmount
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ placeholder "Rate"
                        , value <| String.fromFloat model.loan.ratePerAnnum
                        , onInput ChangeLoanRate
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ placeholder "Duration (months)"
                        , value <| String.fromInt model.loan.term
                        , onInput ChangeLoanTerm
                        ]
                        []
                    ]
                ]
            ]
        ]


viewHouse : Model -> Html Msg
viewHouse model =
    table []
        [ thead []
            [ tr [ colspan 2 ]
                [ text "House" ]
            , tr []
                [ th [] [ text "Value" ]
                , th [] [ text "Rate" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ input
                        [ placeholder "Value"
                        , value <| String.fromFloat model.house.value
                        , onInput ChangeHouseValue
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ placeholder "Market rate"
                        , value <| String.fromFloat model.house.ratePerAnnum
                        , onInput ChangeHouseRate
                        ]
                        []
                    ]
                ]
            ]
        ]


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just e ->
            text e


view : Model -> Html Msg
view model =
    div []
        [ div [ class "error" ] []
        , div []
            [ viewHouse model
            , viewLoan model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
