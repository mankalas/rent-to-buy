module RentToBuy exposing (main)

import Axis
import Browser
import Color exposing (Color)
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
    , f_hr : Field
    , c_hr : Float
    , f_he : Field
    , c_he : Float
    , f_lt : Field
    , c_lt : Int
    , f_lr : Field
    , c_lr : Float

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
        (Field "Market rate (%)" "" Nothing)
        0.0
        (Field "House extras ($)" "" Nothing)
        0.0
        (Field "Loan term (y)" "" Nothing)
        0
        (Field "Loan rate (%)" "" Nothing)
        0.0
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


setError : ( String, List String ) -> Field -> Field
setError e f =
    { f | error = Just (Tuple.first e) }


setValue : String -> Field -> Field
setValue v f =
    { f | value = v }


type alias FField =
    { value : Float }


type alias IField =
    { value : Int }


resetError f =
    { f | error = Nothing }


validateFloatField =
    validate FField |> verify .value (Validators.isFloat "Value must be a float")


validateIntField =
    validate IField |> verify .value (isInt "Value must be an integer")


updateField : Model -> String -> Field -> Validator String { value : String } { value : n } -> (Model -> Field -> Model) -> (Model -> n -> Model) -> Model
updateField m s o_f v up_f up_c =
    let
        n_f =
            o_f |> setValue s
    in
    case v { value = s } of
        Err e ->
            up_f m (n_f |> setError e)

        Ok f ->
            up_f (up_c m f.value) (n_f |> resetError)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        ChangeHouseValue s ->
            ( updateField
                model
                s
                model.f_hv
                validateFloatField
                (\m f -> { m | f_hv = f })
                (\m c -> { m | c_hv = c })
            , Cmd.none
            )

        ChangeHouseRate s ->
            ( updateField
                model
                s
                model.f_hr
                validateFloatField
                (\m f -> { m | f_hr = f })
                (\m c -> { m | c_hr = c })
            , Cmd.none
            )

        ChangeHouseExtras s ->
            ( updateField
                model
                s
                model.f_he
                validateFloatField
                (\m f -> { m | f_he = f })
                (\m c -> { m | c_he = c })
            , Cmd.none
            )

        ChangeLoanAmount s ->
            ( updateField
                model
                s
                model.f_hr
                validateFloatField
                (\m f -> { m | f_hr = f })
                (\m c -> { m | c_hr = c })
            , Cmd.none
            )

        ChangeLoanTerm s ->
            ( updateField
                model
                s
                model.f_lt
                validateIntField
                (\m f -> { m | f_lt = f })
                (\m c -> { m | c_lt = c })
            , Cmd.none
            )

        ChangeLoanRate s ->
            ( updateField
                model
                s
                model.f_lr
                validateFloatField
                (\m f -> { m | f_lr = f })
                (\m c -> { m | c_lr = c })
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


viewField f msg =
    [ dt [] [ text f.name ]
    , dd [] [ input [ value f.value, onInput msg ] [] ]
    , case f.error of
        Nothing ->
            text ""

        Just e ->
            dd [] [ text e ]
    ]


viewHouseForm : Model -> Html Msg
viewHouseForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_hv ChangeHouseValue
                , viewField model.f_hr ChangeHouseRate
                , viewField model.f_he ChangeHouseExtras
                ]
        ]


viewLoanForm : Model -> Html Msg
viewLoanForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_lt ChangeLoanTerm
                , viewField model.f_lr ChangeLoanRate
                ]
        ]


viewCalculusField tt vt =
    [ dt [] [ text tt ]
    , dd [] [ text vt ]
    ]


viewAsDollar f =
    "$ " ++ String.fromFloat f


viewAsPercent f =
    String.fromFloat f ++ " %"


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count == 1 then
        "1 " ++ singular

    else
        String.fromInt count ++ " " ++ plural


viewHouseCalculus : Model -> Html Msg
viewHouseCalculus model =
    div []
        [ dl [] <|
            List.concat
                [ viewCalculusField "House value" <| viewAsDollar model.c_hv
                , viewCalculusField "Market rate" <| viewAsPercent model.c_hr
                , viewCalculusField "House extras" <| viewAsDollar model.c_he
                ]
        ]


viewLoanCalculus : Model -> Html Msg
viewLoanCalculus model =
    div []
        [ dl [] <|
            List.concat
                [ viewCalculusField "Loan term" <| pluralize "year" "years" model.c_lt
                , viewCalculusField "Loan rate" <| viewAsPercent model.c_lr
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
