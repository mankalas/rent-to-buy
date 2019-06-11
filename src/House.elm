module House exposing (Model, updateRate, updateValue)


type alias Model =
    { value : Float
    , ratePerAnnum : Float
    }


updateFloat : String -> Model -> String -> (Float -> Model) -> Result String Model
updateFloat s m err constr =
    case String.toFloat s of
        Nothing ->
            Err err

        Just f ->
            Ok (constr f)


updateRate : String -> Model -> Result String Model
updateRate rate_s model =
    updateFloat rate_s model "Bad house rate" (\r -> Model model.value r)


updateValue : String -> Model -> Result String Model
updateValue rate_s model =
    updateFloat rate_s model "Bad house value" (\v -> Model v model.ratePerAnnum)
