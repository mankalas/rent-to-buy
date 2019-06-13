module RentToBuy exposing (main)

import Axis
import Browser
import Color exposing (Color)
import House as House
import Html exposing (Html, button, dd, div, dl, dt, h1, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, for, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import String.Verify exposing (isInt)
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Validators
import Verify exposing (Validator, validate, verify)



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


type alias Field =
    { name : String
    , value : String
    , error : Maybe String
    }


type alias Model =
    { f_hv : Field
    , c_hv : Float
    , f_lt : Field
    , c_lt : Int

    -- , deposit : Deposit
    -- , loan : Loan
    -- , insurance : Float
    -- , tax : Float
    -- , wPayment : Float
    -- , contract : Contract
    -- , constraint : Constraint
    -- , errors : Maybe (List String)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Field "House value ($)" "" Nothing)
        0.0
        (Field "Loan term (y)" "" Nothing)
        0
      -- (Deposit 10000.0 50)
      -- (Loan 500000 20 0.06)
      -- 3000
      -- 3000
      -- 650.0
      -- (Contract 500000.0 (12 * 3))
      -- HouseC
      -- Nothing
      -- (VerifiedModel
      --     House.initVerified
      -- )
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



-- updateRecord : Model -> Result String m -> (m -> Model) -> Model
-- updateRecord m rec contr =
--     case rec of
--         Err err ->
--             { m | error = Just err }
--         Ok r ->
--             contr r
-- updateHouseRate : Model -> String -> Model
-- updateHouseRate m s =
--     let
--         res_h =
--             updateFloat s m.house "Bad house rate" (\r -> House.model m.house.value r m.house.extras)
--         up new_h =
--             { m | house = new_h }
--         new_m =
--             updateRecord m res_h up
--     in
--     new_m
-- updateHouseValue : Model -> String -> Model
-- updateHouseValue m s =
--     let
--         res_h =
--             updateFloat s m.house "Bad house value" (\v -> House v m.house.ratePerAnnum m.house.extras)
--         up new_h =
--             { m | house = new_h }
--         new_m =
--             updateRecord m res_h up
--     in
--     { new_m | loan = Loan.Model new_m.house.value m.loan.term m.loan.ratePerAnnum }
-- updateLoan : Model -> Result String Loan.Model -> Model
-- updateLoan m l =
--     updateRecord m l (\r -> { m | loan = r })


setHouse : x -> { house : x } -> { house : x }
setHouse h m =
    { m | house = h }


setError : Maybe String -> Field -> Field
setError e f =
    { f | error = e }


setValue : String -> Field -> Field
setValue v f =
    { f | value = v }


type alias FField =
    { value : Float }


type alias IField =
    { value : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        ChangeHouseValue s ->
            ( let
                v =
                    validate FField |> verify .value (Validators.isFloat "Value must be a float")

                n_hv =
                    model.f_hv |> setValue s
              in
              case v { value = s } of
                Err e ->
                    { model | f_hv = n_hv |> setError (Just (Tuple.first e)) }

                Ok f ->
                    { model | f_hv = n_hv |> setError Nothing, c_hv = f.value }
            , Cmd.none
            )

        ChangeHouseRate s ->
            ( model
              --updateHouseValue model s
            , Cmd.none
            )

        ChangeHouseExtras s ->
            ( model
              -- updateHouseExtras model s
            , Cmd.none
            )

        ChangeLoanAmount s ->
            ( model
              -- updateLoanAmount model s
            , Cmd.none
            )

        ChangeLoanTerm s ->
            ( let
                v =
                    validate IField |> verify .value (isInt "Value must be an integer")

                n_lt =
                    model.f_lt |> setValue s
              in
              case v { value = s } of
                Err e ->
                    { model | f_lt = n_lt |> setError (Just (Tuple.first e)) }

                Ok i ->
                    { model | f_lt = n_lt |> setError Nothing, c_lt = i.value }
            , Cmd.none
            )

        ChangeLoanRate s ->
            ( model
              -- updateLoanRate model s
            , Cmd.none
            )

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


recordTable : String -> List String -> List ( Field, String -> Msg ) -> Html Msg
recordTable table_title col_titles v_msg_list =
    let
        col_th t =
            th [] [ text t ]

        col_td ( f, msg ) =
            td []
                [ input
                    [ value f.value
                    , onInput msg
                    ]
                    []
                ]

        col_er ( f, msg ) =
            case f.error of
                Nothing ->
                    text ""

                Just e ->
                    td []
                        [ text e ]
    in
    table []
        [ thead []
            [ tr []
                [ th [ colspan <| List.length col_titles, style "text-align" "center" ] [ text table_title ] ]
            , tr [] <| List.map col_th col_titles
            ]
        , tbody []
            [ tr [] <| List.map col_td v_msg_list
            , tr [] <| List.map col_er v_msg_list
            ]
        ]



-- viewLoan : Model -> Html Msg
-- viewLoan model =
--     recordTable "Loan"
--         [ "Amount", "Rate", "Term (y.)" ]
--         [ ( String.fromFloat model.loan.amount, ChangeLoanAmount )
--         , ( String.fromFloat model.loan.ratePerAnnum, ChangeLoanRate )
--         , ( String.fromInt model.loan.term, ChangeLoanTerm )
--         ]


viewHouseForm : Model -> Html Msg
viewHouseForm model =
    div []
        [ dl []
            [ dt [] [ text model.f_hv.name ]
            , dd [] [ input [ value model.f_hv.value, onInput ChangeHouseValue ] [] ]
            , case model.f_hv.error of
                Nothing ->
                    text ""

                Just e ->
                    dd [] [ text e ]
            ]
        ]


viewLoanForm : Model -> Html Msg
viewLoanForm model =
    div []
        [ dl []
            [ dt [] [ text model.f_lt.name ]
            , dd [] [ input [ value model.f_lt.value, onInput ChangeLoanTerm ] [] ]
            , case model.f_lt.error of
                Nothing ->
                    text ""

                Just e ->
                    dd [] [ text e ]
            ]
        ]



-- viewDeposit : Model -> Html Msg
-- viewDeposit model =
--     recordTable "Deposit"
--         [ "Current", "Weekly" ]
--         [ ( String.fromFloat model.deposit.current, ChangeCurrentDeposit )
--         , ( String.fromFloat model.deposit.wContribution, ChangeWeeklyDeposit )
--         ]
-- viewInsurance : Model -> Html Msg
-- viewInsurance model =
--     recordTable "Insurance"
--         [ "Yearly" ]
--         [ ( String.fromFloat model.insurance, ChangeInsurance ) ]
-- viewTax : Model -> Html Msg
-- viewTax model =
--     recordTable "Tax"
--         [ "Yearly" ]
--         [ ( String.fromFloat model.tax, ChangeTax ) ]
-- viewContract : Model -> Html Msg
-- viewContract model =
--     recordTable "Contract"
--         [ "Amount", "Term" ]
--         [ ( String.fromFloat model.contract.amount, ChangeContractAmount )
--         , ( String.fromInt model.contract.term, ChangeContractTerm )
--         ]
-- viewPayment : Model -> Html Msg
-- viewPayment model =
--     recordTable "Payment"
--         [ "Weekly" ]
--         [ ( String.fromFloat model.wPayment, ChangePayment ) ]
-- w : Float
-- w =
--     900
-- he : Float
-- he =
--     450
-- padding : Float
-- padding =
--     50
-- xScale : ContinuousScale Float
-- xScale =
--     Scale.linear ( 0, w - 2 * padding ) ( now_y, toFloat <| now_y + 20 )
-- yScale : ContinuousScale Float
-- yScale =
--     Scale.linear ( he - 2 * padding, 0 ) ( 0, 500000 )
-- xAxis : List ( Int, Float ) -> Svg msg
-- xAxis model =
--     Axis.bottom [ Axis.tickCount (List.length model) ] xScale
-- yAxis : Svg msg
-- yAxis =
--     Axis.left [ Axis.tickCount 10 ] yScale
-- transformToLineData : ( Int, Float ) -> Maybe ( Float, Float )
-- transformToLineData ( x, y ) =
--     Just ( Scale.convert xScale (toFloat x), Scale.convert yScale y )
-- tranfromToAreaData : ( Int, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
-- tranfromToAreaData ( x, y ) =
--     Just
--         ( ( Scale.convert xScale (toFloat x), Tuple.first (Scale.rangeExtent yScale) )
--         , ( Scale.convert xScale (toFloat x), Scale.convert yScale y )
--         )
-- line : List ( Int, Float ) -> Path
-- line model =
--     List.map transformToLineData model
--         |> Shape.line Shape.monotoneInXCurve
-- area : List ( Int, Float ) -> Path
-- area model =
--     List.map tranfromToAreaData model
--         |> Shape.area Shape.monotoneInXCurve
-- viewGraph : List ( Int, Float ) -> Svg msg
-- viewGraph model =
--     svg [ viewBox 0 0 w he ]
--         [ g [ transform [ Translate (padding - 1) (he - padding) ] ]
--             [ xAxis model ]
--         , g [ transform [ Translate (padding - 1) padding ] ]
--             [ yAxis ]
--         , g [ transform [ Translate padding padding ], class [ "series" ] ]
--             [ Path.element (area model) [ strokeWidth 3, fill <| Fill <| Color.rgba 1 0 0 0.54 ]
--             , Path.element (line model) [ stroke (Color.rgb 1 0 0), strokeWidth 3, fill FillNone ]
--             ]
--         ]
-- now_y =
--     2019
-- weeks_in_year =
--     52.1429
-- timeSeries model =
--     let
--         loan =
--             model.loan
--         pay =
--             model.wPayment
--         l =
--             loan.amount :: List.repeat loan.term 0.0
--         combine n f =
--             ( now_y + n
--             , max 0 <| loan.amount * (1 + loan.ratePerAnnum) - (pay * weeks_in_year * toFloat n)
--             )
--     in
--     List.indexedMap combine l


viewHouseCalculus : Model -> Html Msg
viewHouseCalculus model =
    div []
        [ dl []
            [ dt [] [ text "House value" ]
            , dd [] [ text <| "$ " ++ String.fromFloat model.c_hv ]
            ]
        ]


viewLoanCalculus : Model -> Html Msg
viewLoanCalculus model =
    div []
        [ dl []
            [ dt [] [ text "Loan term" ]
            , dd []
                [ text <|
                    String.fromInt model.c_lt
                        ++ " year"
                        ++ (if model.c_lt > 1 then
                                "s"

                            else
                                ""
                           )
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewHouseForm model
        , viewLoanForm model
        ]


viewCalculus : Model -> Html Msg
viewCalculus model =
    div [ style "border" "solid" ]
        [ viewHouseCalculus model
        , viewLoanCalculus model
        ]


view : Model -> Html Msg
view model =
    table []
        [ tr [ class [ "error" ] ]
            [ td [] [ viewForm model ]
            , td [] [ viewCalculus model ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
