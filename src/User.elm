module User exposing (User, decode, getEmail, getUid)

import Email exposing (..)
import Error exposing (Error)
import Json.Decode as Decode
import Uid exposing (..)


type User
    = User
        { email : Email
        , uid : Uid
        }



-- accessors


getEmail : User -> Email
getEmail (User { email }) =
    email


getUid : User -> Uid
getUid (User { uid }) =
    uid



-- decoding


decode : Decode.Value -> Result Error User
decode json =
    case ( Email.decode json, Uid.decode json ) of
        ( Ok email, Ok uid ) ->
            Ok (User { email = email, uid = uid })

        ( Ok email, Err uid ) ->
            Err uid

        ( Err email, Ok uid ) ->
            Err email

        ( Err email, Err uid ) ->
            Err (Error.combine email uid)
