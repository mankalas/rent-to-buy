module Utils exposing (updateFloat, updateInt)


updateFloat : String -> m -> String -> (Float -> m) -> Result String m
updateFloat s m err constr =
    case String.toFloat s of
        Nothing ->
            Err err

        Just f ->
            Ok (constr f)


updateInt : String -> m -> String -> (Int -> m) -> Result String m
updateInt s m err constr =
    case String.toInt s of
        Nothing ->
            Err err

        Just i ->
            Ok (constr i)
