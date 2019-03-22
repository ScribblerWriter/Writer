module ValidationMessage exposing (ValidationMessage, create, decode, getMessage)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type ValidationMessage
    = ValidationMessage String


create : String -> ValidationMessage
create message =
    ValidationMessage message



-- accessors


getMessage : ValidationMessage -> String
getMessage (ValidationMessage message) =
    message



-- decoding


decode : Decode.Value -> ValidationMessage
decode message =
    Decode.decodeValue Decode.string message
        |> Result.withDefault "Could not decode validation message."
        |> ValidationMessage
