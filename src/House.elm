module House exposing (Model, VerifiedModel, init, initVerified, validator)

import Validators exposing (isFloat)
import Verify exposing (Validator, validate, verify)


type alias Model =
    { value : String
    , ratePerAnnum : String
    , extras : String
    }


type alias VerifiedModel =
    { value : Float
    , ratePerAnnum : Float
    , extras : Float
    }


init : Model
init =
    Model "500000" "0.1" "0"


initVerified =
    VerifiedModel 500000 0.1 0


validator : Validator String Model VerifiedModel
validator =
    validate VerifiedModel
        |> verify .value (isFloat "House value must be a float")
        |> verify .ratePerAnnum (isFloat "House rate must be a float")
        |> verify .extras (isFloat "House extras must be a float")
