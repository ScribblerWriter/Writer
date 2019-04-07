module Uid exposing (Uid, decode, encode, getUid)

import Error exposing (Error)
import ErrorEntry exposing (ErrorEntry)
import Json.Decode as Decode
import Json.Encode as Encode


type Uid
    = Uid String



-- accessors


getUid : Uid -> String
getUid (Uid uid) =
    uid



-- coding


decode : Decode.Value -> Result Error Uid
decode uid =
    case Decode.decodeValue uidDecoder uid of
        Ok decoded ->
            Ok decoded

        Err error ->
            Err
                (Decode.errorToString error
                    |> Error.create "An error occurred while decoding uid."
                )


uidDecoder : Decode.Decoder Uid
uidDecoder =
    Decode.field "uid" Decode.string
        |> Decode.andThen (\uid -> Decode.succeed (Uid uid))


encode : Uid -> Encode.Value
encode (Uid uid) =
    Encode.string uid
