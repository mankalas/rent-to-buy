module Main exposing (Contract, Deposit, FField, Field, IField, Insurance, InterestRate, Mode(..), Model, Msg(..), Tax, init, main, pluralize, resetError, setError, setValue, subscriptions, update, updateField, validateFloatField, validateIntField, view, viewAsDollar, viewAsPercent, viewCalculus, viewCalculusField, viewField, viewForm, viewHouseCalculus, viewHouseForm, viewLoanCalculus, viewLoanForm, viewMode)

import Browser
import Chart
import Html exposing (Html, button, dd, div, dl, dt, fieldset, h1, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, colspan, disabled, for, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Loan exposing (..)
import Round
import String.Verify exposing (isInt)
import Validators
import Verify exposing (Validator, validate, verify)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Mode
    = House
    | Payment


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
    , f_lr : Field
    , loan : Loan.Model
    , f_pay : Field
    , c_pay : Float
    , f_ct : Field
    , c_ct : Int
    , mode : Mode

    -- , deposit : Deposit
    -- , loan : Loan
    -- , insurance : Float
    -- , tax : Float
    -- , wPayment : Float
    -- , contract : Contract
    -- , mode : Mode
    -- , errors : Maybe (List String)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( updateCalculations <|
        Model
            (Field "House value ($)" "500000" Nothing)
            500000.0
            (Field "Market rate (%)" "10" Nothing)
            10
            (Field "House extras ($)" "1000" Nothing)
            1000
            (Field "Loan term (y)" "20" Nothing)
            (Field "Loan rate (%)" "5" Nothing)
            (Loan.Model 20 5 500000)
            (Field "Payment ($/w)" "500" Nothing)
            500.0
            (Field "Contract term (y)" "3" Nothing)
            3
            House
      -- Loan amount
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
    | ChangeModeToHouse
    | ChangeModeToPayment


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
            updateCalculations <| up_f (up_c m f.value) (n_f |> resetError)


updateCalculations : Model -> Model
updateCalculations m =
    let
        up_la =
            m.c_hv + m.c_he

        old_loan =
            m.loan

        new_loan =
            { old_loan | amount = m.c_hv + m.c_he }

        pay =
            wPayments new_loan

        up_pay =
            if isModeHouse m then
                pay

            else
                m.c_pay

        up_loan =
            interests new_loan
    in
    { m | loan = new_loan, c_pay = up_pay }


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
                (\m c ->
                    let
                        l =
                            m.loan

                        nl =
                            { l | term = c }
                    in
                    { m | loan = nl }
                )
            , Cmd.none
            )

        ChangeLoanRate s ->
            ( updateField
                model
                s
                model.f_lr
                validateFloatField
                (\m f -> { m | f_lr = f })
                (\m c ->
                    let
                        l =
                            m.loan

                        nl =
                            { l | interest_rate = c }
                    in
                    { m | loan = nl }
                )
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
            ( updateField
                model
                s
                model.f_pay
                validateFloatField
                (\m f -> { m | f_pay = f })
                (\m c -> { m | c_pay = c })
            , Cmd.none
            )

        ChangeContractAmount s ->
            ( model, Cmd.none )

        ChangeContractTerm s ->
            ( updateField
                model
                s
                model.f_ct
                validateIntField
                (\m f -> { m | f_ct = f })
                (\m i -> { m | c_ct = i })
            , Cmd.none
            )

        ChangeModeToHouse ->
            ( updateCalculations { model | mode = House }
            , Cmd.none
            )

        ChangeModeToPayment ->
            ( updateCalculations { model | mode = Payment }
            , Cmd.none
            )



-- VIEW


viewMode : Model -> Html Msg
viewMode model =
    div []
        [ label [ for "h_mode" ] [ text "House" ]
        , input
            [ type_ "radio", name "p_mode" ]
            []
        , label [ for "p_mode" ] [ text "Payment" ]
        , input
            [ type_ "radio", name "h_mode" ]
            []
        ]


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
                [ if isModeHouse model then
                    viewField model.f_hv ChangeHouseValue

                  else
                    [ text "" ]
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


viewPaymentForm : Model -> Html Msg
viewPaymentForm model =
    div []
        [ dl [] <|
            List.concat
                [ if isModeHouse model then
                    [ text "" ]

                  else
                    viewField model.f_pay ChangePayment
                ]
        ]


viewContractForm : Model -> Html Msg
viewContractForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_ct ChangeContractTerm ]
        ]


viewCalculusField tt vt =
    [ dt [] [ text tt ]
    , dd [] [ text vt ]
    ]


viewAsDollar f =
    "$ " ++ Round.round 2 f


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
                [ viewCalculusField "Loan term" <| pluralize "year" "years" model.loan.term
                , viewCalculusField "Loan rate" <| viewAsPercent model.loan.interest_rate
                , viewCalculusField "Loan amount" <| viewAsDollar model.loan.amount
                , viewCalculusField "Loan total" <| viewAsDollar (Loan.total model.loan)
                , viewCalculusField "Loan interests" <| viewAsDollar (Loan.interests model.loan)
                ]
        ]


viewPaymentCalculus : Model -> Html Msg
viewPaymentCalculus model =
    div []
        [ dl [] <|
            List.concat
                [ viewCalculusField "Weekly payment" <| viewAsDollar model.c_pay
                ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewHouseForm model
        , viewLoanForm model
        , viewPaymentForm model
        , viewContractForm model
        ]


t : Model -> Chart.Model
t m =
    { hv = m.c_hv
    , hr = m.c_hr
    , lt = m.loan.term
    }


viewCalculus : Model -> Html Msg
viewCalculus model =
    div [ style "border" "solid" ]
        [ viewHouseCalculus model
        , viewLoanCalculus model
        , viewPaymentCalculus model
        , Chart.view (t model)
        ]


viewLandlord : Model -> Html Msg
viewLandlord model =
    div [ style "border" "solid" ]
        [ ul []
            [ li [] [ text <| "Borrows " ++ viewAsDollar model.loan.amount ]
            , li [] [ text <| "Pays " ++ viewAsDollar model.c_pay ]
            , li [] [ text <| "After " ++ pluralize "year" "years" model.c_ct ++ "..." ]
            , li [] [ text <| "Has to reimburse " ++ (viewAsDollar <| (model.loan.amount - Loan.interestsAt model.loan (Loan.wPayments model.loan) (52 * (model.loan.term - model.c_ct)))) ]
            ]
        ]


viewTenant : Model -> Html Msg
viewTenant model =
    div [ style "border" "solid" ]
        [ text "t" ]


isModeHouse : Model -> Bool
isModeHouse m =
    case m.mode of
        House ->
            True

        _ ->
            False


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ div []
                [ h1 [] [ text "Test Chart" ]
                , fieldset []
                    [ label [ for "mode_h" ] [ text "House" ]
                    , input
                        [ type_ "radio"
                        , name "mode_h"
                        , checked <| isModeHouse model
                        , onClick ChangeModeToHouse
                        ]
                        []
                    , label [ for "mode_p" ] [ text "Payment" ]
                    , input
                        [ type_ "radio"
                        , name "mode_p"
                        , checked <| not (isModeHouse model)
                        , onClick ChangeModeToPayment
                        ]
                        []
                    ]
                , table []
                    [ tr [ class "error" ]
                        [ td [] [ viewForm model ]
                        , td [] [ viewCalculus model ]
                        , td [] [ viewLandlord model ]
                        , td [] [ viewTenant model ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Rent to buy"
    , body = body
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
