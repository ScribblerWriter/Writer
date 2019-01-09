port module Ports exposing (Message, Operation(..), incomingMessage, sendMessage, stringToOperation)

import Json.Encode as Encode


type alias Message =
    { operation : String
    , content : Maybe Encode.Value
    }


port outgoingMessage : Message -> Cmd msg


port incomingMessage : (Message -> msg) -> Sub msg


type Operation
    = Unknown
    | SaveContent
    | LoadContent
    | ContentLoaded



-- Port operations


sendMessage : Operation -> Maybe Encode.Value -> Cmd msg
sendMessage operation content =
    outgoingMessage
        { operation = operationToString operation
        , content = content
        }



-- Operations


operationToString : Operation -> String
operationToString operation =
    case operation of
        SaveContent ->
            "SaveContent"

        LoadContent ->
            "LoadContent"

        _ ->
            "Unknown"


stringToOperation : String -> Operation
stringToOperation operation =
    case operation of
        "ContentLoaded" ->
            ContentLoaded

        _ ->
            Unknown
