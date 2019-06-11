module RentToBuy exposing (main)

import Browser
import House as House exposing (Model, updateRate, updateValue)
import Html exposing (Html, button, div, h1, input, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http



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


type alias InterestRate =
    { ratePerAnnum : Float
    }


type alias Loan =
    { amount : Float
    , term : Int
    , rate : InterestRate
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
    , loan : Loan
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
        (Loan 500000 (12 * 20) (InterestRate 0.06))
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


updateModelWithFloat : Model -> String -> String -> (Model -> Float -> Model) -> Model
updateModelWithFloat m s e u =
    let
        f =
            String.toFloat s
    in
    case f of
        Nothing ->
            { m | error = Just e }

        Just v ->
            u m v


updateHouse : Model -> Result String House.Model -> Model
updateHouse model res_house =
    case res_house of
        Err err ->
            { model | error = Just err }

        Ok house ->
            { model | house = house }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        ChangeHouseRate s ->
            ( updateHouse model <| House.updateRate s model.house, Cmd.none )

        ChangeHouseValue s ->
            ( updateHouse model <| House.updateValue s model.house, Cmd.none )



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
                [ td [] [ input [] [] ]
                , td [] [ input [] [] ]
                , td [] [ input [] [] ]
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
                        [ placeholder "House value"
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
