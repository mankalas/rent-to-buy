module RentToBuy exposing (main)

import Axis
import Browser
import Color exposing (Color)
import House as House
import Html exposing (Html, button, div, h1, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, for, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))



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


type alias Loan =
    { amount : Float
    , term : Int
    , ratePerAnnum : Float
    }


type alias InterestRate =
    { ratePerAnnum : Float
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
    { house : House.Model
    , deposit : Deposit
    , loan : Loan
    , insurance : Float
    , tax : Float
    , wPayment : Float
    , contract : Contract
    , constraint : Constraint
    , errors : Maybe (List String)
    }


type alias VerifiedModel =
    { house : House.VerifiedModel
    , deposit : Deposit
    , loan : Loan
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
        House.init
        (Deposit 10000.0 50)
        (Loan 500000 20 0.06)
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
            updateFloat s m.house "Bad house rate" (\r -> House.model m.house.value r m.house.extras)

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
            ( let
                old_house =
                    model.house

                new_house =
                    { old_house | rate = s }

                result_house =
                    House.validator new_house
              in
              case result_house of
                Err err ->
                    { model | errors = err }

                Ok verified_house ->
                    let
                        old_verified_model =
                            model.verified

                        new_verified_model =
                            { old_verified_model | house = verified_house }
                    in
                    { model | verified = new_verified_model }
            , Cmd.none
            )

        ChangeHouseExtras s ->
            ( updateHouseExtras model s, Cmd.none )

        ChangeHouseValue s ->
            ( updateHouseValue model s, Cmd.none )

        ChangeLoanAmount s ->
            ( updateLoanAmount model s, Cmd.none )

        ChangeLoanTerm s ->
            ( updateLoanTerm model s, Cmd.none )

        ChangeLoanRate s ->
            ( updateLoanRate model s, Cmd.none )

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


recordTable : String -> List String -> List ( String, String -> Msg ) -> Html Msg
recordTable table_title col_titles v_msg_list =
    let
        col_th t =
            th [] [ text t ]

        col_td ( v, msg ) =
            td []
                [ input
                    [ value v
                    , onInput msg
                    ]
                    []
                ]
    in
    table []
        [ thead []
            [ tr []
                [ th [ colspan <| List.length col_titles, style "text-align" "center" ] [ text table_title ] ]
            , tr [] <| List.map col_th col_titles
            ]
        , tbody []
            [ tr [] <| List.map col_td v_msg_list
            ]
        ]


viewLoan : Model -> Html Msg
viewLoan model =
    recordTable "Loan"
        [ "Amount", "Rate", "Term (y.)" ]
        [ ( String.fromFloat model.loan.amount, ChangeLoanAmount )
        , ( String.fromFloat model.loan.ratePerAnnum, ChangeLoanRate )
        , ( String.fromInt model.loan.term, ChangeLoanTerm )
        ]


viewHouse : Model -> Html Msg
viewHouse model =
    recordTable "House"
        [ "Value", "Market Rate", "Extras" ]
        [ ( String.fromFloat model.house.value, ChangeHouseValue )
        , ( String.fromFloat model.house.ratePerAnnum, ChangeHouseRate )
        , ( String.fromFloat model.house.extras, ChangeHouseExtras )
        ]


viewDeposit : Model -> Html Msg
viewDeposit model =
    recordTable "Deposit"
        [ "Current", "Weekly" ]
        [ ( String.fromFloat model.deposit.current, ChangeCurrentDeposit )
        , ( String.fromFloat model.deposit.wContribution, ChangeWeeklyDeposit )
        ]


viewInsurance : Model -> Html Msg
viewInsurance model =
    recordTable "Insurance"
        [ "Yearly" ]
        [ ( String.fromFloat model.insurance, ChangeInsurance ) ]


viewTax : Model -> Html Msg
viewTax model =
    recordTable "Tax"
        [ "Yearly" ]
        [ ( String.fromFloat model.tax, ChangeTax ) ]


viewContract : Model -> Html Msg
viewContract model =
    recordTable "Contract"
        [ "Amount", "Term" ]
        [ ( String.fromFloat model.contract.amount, ChangeContractAmount )
        , ( String.fromInt model.contract.term, ChangeContractTerm )
        ]


viewPayment : Model -> Html Msg
viewPayment model =
    recordTable "Payment"
        [ "Weekly" ]
        [ ( String.fromFloat model.wPayment, ChangePayment ) ]


w : Float
w =
    900


he : Float
he =
    450


padding : Float
padding =
    50


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( now_y, toFloat <| now_y + 20 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( he - 2 * padding, 0 ) ( 0, 500000 )


xAxis : List ( Int, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 10 ] yScale


transformToLineData : ( Int, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale (toFloat x), Scale.convert yScale y )


tranfromToAreaData : ( Int, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale (toFloat x), Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale (toFloat x), Scale.convert yScale y )
        )


line : List ( Int, Float ) -> Path
line model =
    List.map transformToLineData model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Int, Float ) -> Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve


viewGraph : List ( Int, Float ) -> Svg msg
viewGraph model =
    svg [ viewBox 0 0 w he ]
        [ g [ transform [ Translate (padding - 1) (he - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Fill <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line model) [ stroke (Color.rgb 1 0 0), strokeWidth 3, fill FillNone ]
            ]
        ]


now_y =
    2019


weeks_in_year =
    52.1429


timeSeries model =
    let
        loan =
            model.loan

        pay =
            model.wPayment

        l =
            loan.amount :: List.repeat loan.term 0.0

        combine n f =
            ( now_y + n
            , max 0 <| loan.amount * (1 + loan.ratePerAnnum) - (pay * weeks_in_year * toFloat n)
            )
    in
    List.indexedMap combine l


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
            , viewGraph <| timeSeries model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
