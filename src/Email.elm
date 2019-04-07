module Email exposing (Email, create, decode, encode, getEmail)

import Error exposing (Error)
import ErrorEntry exposing (ErrorEntry)
import Json.Decode as Decode
import Json.Encode as Encode


type Email
    = Email String


create : String -> Email
create email =
    Email email



-- accessors


getEmail : Email -> String
getEmail (Email email) =
    email



-- coding


decode : Decode.Value -> Result Error Email
decode email =
    case Decode.decodeValue emailDecoder email of
        Ok decoded ->
            Ok decoded

        Err error ->
            Err
                (Decode.errorToString error
                    |> Error.create "An error occurred while decoding email."
                )


emailDecoder : Decode.Decoder Email
emailDecoder =
    Decode.field "email" Decode.string
        |> Decode.andThen (\email -> Decode.succeed (Email email))


encode : Email -> Encode.Value
encode (Email email) =
    Encode.string email
