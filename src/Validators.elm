module Validators exposing (isFloat)

import Verify exposing (Validator)


isFloat : error -> Validator error String Float
isFloat error =
    String.toFloat >> Result.fromMaybe ( error, [] )
