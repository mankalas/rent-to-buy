module Main exposing (main)

import Browser
import Chart
import Html exposing (Html, button, dd, div, dl, dt, fieldset, h1, h2, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
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
    , f_cd : Field
    , c_cd : Float
    , f_tax : Field
    , c_tax : Float
    , f_insur : Field
    , c_insur : Float
    , f_twd : Field
    , c_twd : Float

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
            (Field "House value ($)" "450000" Nothing)
            450000.0
            (Field "Market rate (%)" "10" Nothing)
            0.1
            (Field "House extras ($)" "0" Nothing)
            0
            (Field "Loan term (y)" "25" Nothing)
            (Field "Loan rate (%)" "5" Nothing)
            (Loan.Model (52 * 25) 0.05 500000)
            (Field "Payment ($/w)" "500" Nothing)
            500.0
            (Field "Contract term (y)" "3" Nothing)
            3
            (Field "Contract deposit ($)" "10000" Nothing)
            10000
            (Field "Tax ($/y)" "1000" Nothing)
            1000
            (Field "Insurance ($/y)" "1000" Nothing)
            1000
            (Field "Tenant deposit ($/w)" "100" Nothing)
            100
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
    | ChangeLoanRate String
    | ChangeLoanTerm String
    | ChangeCurrentDeposit String
    | ChangeWeeklyDeposit String
    | ChangeInsurance String
    | ChangeTax String
    | ChangePayment String
    | ChangeContractTerm String
    | ChangeContractDeposit String
    | ChangeTenantDeposit String


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
            calculateLoanAmount m

        old_loan =
            m.loan

        new_loan =
            { old_loan | amount = up_la }
    in
    { m
        | loan = new_loan
        , c_pay = weeklyPayment m
    }


weeklyPayment : Model -> Float
weeklyPayment model =
    Loan.payment model.loan + (model.c_insur + model.c_tax) / 52


calculateLoanAmount : Model -> Float
calculateLoanAmount model =
    model.c_hv + model.c_he - model.c_cd


builtDeposit : Model -> Float
builtDeposit model =
    model.c_twd * 52 * toFloat model.c_ct


houseCapitalGain : Model -> Float
houseCapitalGain model =
    model.c_hv * ((1 + model.c_hr) ^ toFloat model.c_ct) - model.c_hv


remainingLoan : Model -> Float
remainingLoan model =
    Loan.amortBegBalance model.loan (52 * model.c_ct)


roiAmount : Model -> Float
roiAmount model =
    model.loan.amount - remainingLoan model


roiPercent : Model -> Float
roiPercent model =
    roiAmount model / model.c_cd


weeklySpending : Model -> Float
weeklySpending model =
    model.c_pay + model.c_twd


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
                (\m c -> { m | c_hr = c / 100 })
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
                            { l | term = 52 * c }
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
                            { l | interest_rate = c / 100 }
                    in
                    { m | loan = nl }
                )
            , Cmd.none
            )

        ChangeCurrentDeposit s ->
            ( model, Cmd.none )

        ChangeWeeklyDeposit s ->
            ( model, Cmd.none )

        ChangeTax s ->
            ( updateField
                model
                s
                model.f_tax
                validateFloatField
                (\m f -> { m | f_tax = f })
                (\m c -> { m | c_tax = c })
            , Cmd.none
            )

        ChangeInsurance s ->
            ( updateField
                model
                s
                model.f_insur
                validateFloatField
                (\m f -> { m | f_insur = f })
                (\m c -> { m | c_insur = c })
            , Cmd.none
            )

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

        ChangeContractDeposit s ->
            ( updateField
                model
                s
                model.f_cd
                validateFloatField
                (\m f -> { m | f_cd = f })
                (\m c -> { m | c_cd = c })
            , Cmd.none
            )

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

        ChangeTenantDeposit s ->
            ( updateField
                model
                s
                model.f_twd
                validateFloatField
                (\m f -> { m | f_twd = f })
                (\m c -> { m | c_twd = c })
            , Cmd.none
            )



-- VIEW


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


viewContractForm : Model -> Html Msg
viewContractForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_ct ChangeContractTerm
                , viewField model.f_cd ChangeContractDeposit
                ]
        ]


viewMaintenanceForm : Model -> Html Msg
viewMaintenanceForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_tax ChangeTax
                , viewField model.f_insur ChangeInsurance
                ]
        ]


viewTenantForm : Model -> Html Msg
viewTenantForm model =
    div []
        [ dl [] <|
            List.concat
                [ viewField model.f_twd ChangeTenantDeposit
                ]
        ]


viewCalculusField tt vt =
    [ dt [] [ text tt ]
    , dd [] [ text vt ]
    ]


viewAsDollar f =
    "$" ++ Round.round 2 f


viewAsPercent f =
    Round.round 2 (100 * f) ++ " %"


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
                [ viewCalculusField "Loan term" <| pluralize "year" "years" (model.loan.term // 52)
                , viewCalculusField "Loan rate" <| viewAsPercent model.loan.interest_rate
                , viewCalculusField "Loan amount" <| viewAsDollar model.loan.amount
                , viewCalculusField "Loan interests" <| viewAsDollar (Loan.interest model.loan)
                , viewCalculusField "Loan total" <| viewAsDollar (Loan.cost model.loan)
                ]
        ]


viewPaymentCalculus : Model -> Html Msg
viewPaymentCalculus model =
    div []
        [ dl [] <|
            List.concat
                [ viewCalculusField "Weekly payment (loan)" <| viewAsDollar (Loan.payment model.loan)
                , viewCalculusField "Weekly payment (total)" <| viewAsDollar model.c_pay
                ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewHouseForm model
        , viewLoanForm model
        , viewContractForm model
        , viewMaintenanceForm model
        , viewTenantForm model
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
        [ h2 [] [ text "Landlord" ]
        , ul []
            [ li [] [ text <| "Borrows " ++ viewAsDollar model.loan.amount ]
            , li [] [ text <| "Pays " ++ viewAsDollar model.c_pay ++ " per week" ]
            ]
        , h2 [] [ text <| "After " ++ pluralize "year" "years" model.c_ct ]
        , ul []
            [ li [] [ text <| "Left to pay " ++ viewAsDollar (remainingLoan model) ]
            , li [] [ text <| "ROI " ++ viewAsDollar (roiAmount model) ++ " (" ++ viewAsPercent (roiPercent model) ++ ")" ]
            ]
        ]


viewTenant : Model -> Html Msg
viewTenant model =
    div [ style "border" "solid" ]
        [ h2 [] [ text "Tenant" ]
        , ul []
            [ li [] [ text <| "Spends " ++ viewAsDollar (weeklySpending model) ++ " per week" ]
            , li [] [ text <| "Including " ++ viewAsDollar model.c_twd ++ " deposit per week" ]
            ]
        , h2 [] [ text <| "After " ++ pluralize "year" "years" model.c_ct ]
        , ul []
            [ li [] [ text <| "Cash deposit of " ++ viewAsDollar (builtDeposit model) ]
            , li [] [ text <| "House value increased by " ++ viewAsDollar (houseCapitalGain model) ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ div []
                [ h1 [] [ text "Rent-to-buy scheme" ]
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
