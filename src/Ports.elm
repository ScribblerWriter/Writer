port module Ports exposing (Message, incomingMessage, sendMessage)

import Json.Encode as Encode


type alias Message =
    { operation : String
    , content : Maybe Encode.Value
    }


port outgoingMessage : Message -> Cmd msg


port incomingMessage : (Message -> msg) -> Sub msg



-- Port operations


sendMessage : String -> Maybe Encode.Value -> Cmd msg
sendMessage operation content =
    outgoingMessage { operation = operation, content = content }
