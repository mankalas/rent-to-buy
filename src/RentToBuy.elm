module RentToBuy exposing (main)

import Axis
import Browser
import Color exposing (Color)
import Html exposing (Html, button, div, h1, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, for, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Loan as Loan exposing (Model, updateAmount, updateRate, updateTerm)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Utils exposing (updateFloat)



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


type alias House =
    { value : Float
    , ratePerAnnum : Float
    , extras : Float
    }


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
    { house : House
    , deposit : Deposit
    , loan : Loan.Model
    , insurance : Float
    , tax : Float
    , wPayment : Float
    , contract : Contract
    , constraint : Constraint
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (House 500000.0 0.1 0)
        (Deposit 10000.0 50)
        (Loan.Model 500000 (12 * 20) 0.06)
        3000
        3000
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
    | ChangeHouseExtras String
    | ChangeHouseValue String
    | ChangeLoanAmount String
    | ChangeLoanRate String
    | ChangeLoanTerm String
    | ChangeCurrentDeposit String
    | ChangeWeeklyDeposit String
    | ChangeInsurance String
    | ChangeTax String
    | ChangePayment String
    | ChangeContractAmount String
    | ChangeContractTerm String


updateRecord : Model -> Result String m -> (m -> Model) -> Model
updateRecord m rec contr =
    case rec of
        Err err ->
            { m | error = Just err }

        Ok r ->
            contr r


updateHouseRate : Model -> String -> Model
updateHouseRate m s =
    let
        res_h =
            updateFloat s m.house "Bad house rate" (\r -> House m.house.value r m.house.extras)

        up new_h =
            { m | house = new_h }

        new_m =
            updateRecord m res_h up
    in
    new_m


updateHouseValue : Model -> String -> Model
updateHouseValue m s =
    let
        res_h =
            updateFloat s m.house "Bad house value" (\v -> House v m.house.ratePerAnnum m.house.extras)

        up new_h =
            { m | house = new_h }

        new_m =
            updateRecord m res_h up
    in
    { new_m | loan = Loan.Model new_m.house.value m.loan.term m.loan.ratePerAnnum }


updateLoan : Model -> Result String Loan.Model -> Model
updateLoan m l =
    updateRecord m l (\r -> { m | loan = r })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        ChangeHouseRate s ->
            ( updateHouseRate model s, Cmd.none )

        ChangeHouseExtras s ->
            ( updateHouseRate model s, Cmd.none )

        ChangeHouseValue s ->
            ( updateHouseValue model s, Cmd.none )

        ChangeLoanAmount s ->
            ( updateLoan model <| Loan.updateAmount s model.loan, Cmd.none )

        ChangeLoanTerm s ->
            ( updateLoan model <| Loan.updateTerm s model.loan, Cmd.none )

        ChangeLoanRate s ->
            ( updateLoan model <| Loan.updateRate s model.loan, Cmd.none )

        ChangeCurrentDeposit s ->
            ( model, Cmd.none )

        ChangeWeeklyDeposit s ->
            ( model, Cmd.none )

        ChangeInsurance s ->
            ( model, Cmd.none )

        ChangeTax s ->
            ( model, Cmd.none )

        ChangePayment s ->
            ( model, Cmd.none )

        ChangeContractAmount s ->
            ( model, Cmd.none )

        ChangeContractTerm s ->
            ( model, Cmd.none )



-- VIEW


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just e ->
            text e


viewConstraint : Model -> Html Msg
viewConstraint model =
    div []
        [ label [ for "h_constraint" ] [ text "House" ]
        , input
            [ type_ "radio", name "p_constraint" ]
            []
        , label [ for "p_constraint" ] [ text "Payment" ]
        , input
            [ type_ "radio", name "h_constraint" ]
            []
        ]


viewLoan : Model -> Html Msg
viewLoan model =
    table []
        [ thead []
            [ tr [] [ th [ colspan 2, style "text-align" "center" ] [ text "Loan" ] ]
            , tr []
                [ th [] [ text "Amount" ]
                , th [] [ text "Rate" ]
                , th [] [ text "Term (m.)" ]
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
                        [ placeholder "Term (months)"
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
            [ tr []
                [ th [ colspan 2, style "text-align" "center" ] [ text "House" ] ]
            , tr []
                [ th [] [ text "Value" ]
                , th [] [ text "Rate" ]
                , th [] [ text "Extras" ]
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
                , td []
                    [ input
                        [ placeholder "Extras"
                        , value <| String.fromFloat model.house.extras
                        , onInput ChangeHouseExtras
                        ]
                        []
                    ]
                ]
            ]
        ]


viewDeposit : Model -> Html Msg
viewDeposit model =
    table []
        [ thead []
            [ tr [] [ th [ style "text-align" "center", colspan 2 ] [ text "Deposit" ] ]
            , tr []
                [ th [] [ text "Current" ]
                , th [] [ text "Weekly" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ input
                        [ placeholder "Deposit"
                        , value <| String.fromFloat model.deposit.current
                        , onInput ChangeCurrentDeposit
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ placeholder "Weekly Deposit"
                        , value <| String.fromFloat model.deposit.wContribution
                        , onInput ChangeWeeklyDeposit
                        ]
                        []
                    ]
                ]
            ]
        ]


viewInsurance : Model -> Html Msg
viewInsurance model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Insurance (yearly)" ] ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ input
                        [ placeholder "Insurance"
                        , value <| String.fromFloat model.insurance
                        , onInput ChangeInsurance
                        ]
                        []
                    ]
                ]
            ]
        ]


viewTax : Model -> Html Msg
viewTax model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Tax (yearly)" ] ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ input
                        [ placeholder "Tax"
                        , value <| String.fromFloat model.tax
                        , onInput ChangeTax
                        ]
                        []
                    ]
                ]
            ]
        ]


viewContract : Model -> Html Msg
viewContract model =
    div []
        [ h1 [] [ text "Contract" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Amount" ]
                    , th [] [ text "Term" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ input
                            [ placeholder "Amount"
                            , value <| String.fromFloat model.contract.amount
                            , onInput ChangeContractAmount
                            ]
                            []
                        ]
                    , td []
                        [ input
                            [ placeholder "Term"
                            , value <| String.fromInt model.contract.term
                            , onInput ChangeContractTerm
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


viewPayment : Model -> Html Msg
viewPayment model =
    div []
        [ h1 [] [ text "Payment" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Weekly" ] ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ input
                            [ placeholder "Payment"
                            , value <| String.fromFloat model.wPayment
                            , onInput ChangePayment
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale Time.Posix
xScale =
    Scale.time Time.utc ( 0, w - 2 * padding ) ( Time.millisToPosix 1560377806000, Time.millisToPosix 1560379506000 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 5 )


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


tranfromToAreaData : ( Time.Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


line : List ( Time.Posix, Float ) -> Path
line model =
    List.map transformToLineData model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float ) -> Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve


viewGraph : List ( Time.Posix, Float ) -> Svg msg
viewGraph model =
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Fill <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line model) [ stroke (Color.rgb 1 0 0), strokeWidth 3, fill FillNone ]
            ]
        ]


timeSeries =
    [ ( Time.millisToPosix 1560377806000, 4.3 )
    , ( Time.millisToPosix 1560379106000, 6.3 )
    , ( Time.millisToPosix 1560379506000, 41.3 )
    ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class [ "error" ] ] []
        , div []
            [ viewConstraint model
            , viewHouse model
            , viewLoan model
            , viewDeposit model
            , viewInsurance model
            , viewTax model
            , viewContract model
            , viewPayment model
            , viewGraph timeSeries
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
