port module Ports exposing
    ( InMessage
    , InOperation(..)
    , OutOperation(..)
    , incomingMessage
    , sendJustMessage
    , sendMessageWithContent
    , sendMessageWithContentAndResponse
    , sendMessageWithJustResponse
    , stringToOperation
    )

import Json.Encode as Encode


type alias OutMessage =
    { operation : String
    , returnOperation : Maybe String
    , content : Maybe Encode.Value
    }


type alias InMessage =
    { operation : String
    , content : Maybe Encode.Value
    }


port outgoingMessage : OutMessage -> Cmd msg


port incomingMessage : (InMessage -> msg) -> Sub msg


type OutOperation
    = SaveContent
    | LoadContent


type InOperation
    = Unknown
    | ContentLoaded



-- Port operations


sendJustMessage : OutOperation -> Cmd msg
sendJustMessage operation =
    sendMessageWithContentAndResponse operation Nothing Nothing


sendMessageWithContent : OutOperation -> Encode.Value -> Cmd msg
sendMessageWithContent operation content =
    sendMessageWithContentAndResponse operation Nothing (Just content)


sendMessageWithJustResponse : OutOperation -> InOperation -> Cmd msg
sendMessageWithJustResponse operation returnOperation =
    sendMessageWithContentAndResponse operation (Just returnOperation) Nothing


sendMessageWithContentAndResponse : OutOperation -> Maybe InOperation -> Maybe Encode.Value -> Cmd msg
sendMessageWithContentAndResponse operation returnOperation content =
    outgoingMessage
        { operation = outOperationToString operation
        , returnOperation =
            case returnOperation of
                Just op ->
                    Just (inOperationToString op)

                Nothing ->
                    Nothing
        , content = content
        }



-- Conversions


outOperationToString : OutOperation -> String
outOperationToString operation =
    case operation of
        SaveContent ->
            "SaveContent"

        LoadContent ->
            "LoadContent"


inOperationToString : InOperation -> String
inOperationToString operation =
    case operation of
        ContentLoaded ->
            "ContentLoaded"

        Unknown ->
            "Unknown"


stringToOperation : String -> InOperation
stringToOperation operation =
    case operation of
        "ContentLoaded" ->
            ContentLoaded

        _ ->
            Unknown
