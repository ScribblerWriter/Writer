port module Ports exposing
    ( InMessage
    , InOperation(..)
    , OutOperation(..)
    , incomingMessage
    , sendJustMessage
    , sendMessageWithContentAndResponse
    , sendMessageWithJustContent
    , sendMessageWithJustResponse
    , stringToInOperation
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
    | QueryDbMultiple
    | QueryDbSingle
    | SaveToDbCollection
    | SaveToDbSubcollection
    | SignIn
    | SignOut
    | SignUp


type InOperation
    = Unknown
    | ContentLoaded
    | TargetListReturned
    | AuthStateChanged
    | SettingsLoaded
    | SettingsSaved
    | DisplayMessageReceived
    | WriterDataSaved



-- Port operations


sendJustMessage : OutOperation -> Cmd msg
sendJustMessage operation =
    sendMessage operation Nothing Nothing


sendMessageWithJustContent : OutOperation -> Encode.Value -> Cmd msg
sendMessageWithJustContent operation content =
    sendMessage operation (Just content) Nothing


sendMessageWithJustResponse : OutOperation -> InOperation -> Cmd msg
sendMessageWithJustResponse operation returnOperation =
    sendMessage operation Nothing (Just returnOperation)


sendMessageWithContentAndResponse : OutOperation -> Encode.Value -> InOperation -> Cmd msg
sendMessageWithContentAndResponse operation content returnOperation =
    sendMessage operation (Just content) (Just returnOperation)


sendMessage : OutOperation -> Maybe Encode.Value -> Maybe InOperation -> Cmd msg
sendMessage operation content returnOperation =
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

        QueryDbMultiple ->
            "QueryDbMultiple"

        QueryDbSingle ->
            "QueryDbSingle"

        SignIn ->
            "SignIn"

        SignOut ->
            "SignOut"

        SignUp ->
            "SignUp"

        SaveToDbCollection ->
            "SaveToDbCollection"

        SaveToDbSubcollection ->
            "SaveToDbSubcollection"


inOperationToString : InOperation -> String
inOperationToString operation =
    case operation of
        ContentLoaded ->
            "ContentLoaded"

        Unknown ->
            "Unknown"

        TargetListReturned ->
            "TargetListReturned"

        AuthStateChanged ->
            "AuthStateChanged"

        SettingsLoaded ->
            "SettingsLoaded"

        SettingsSaved ->
            "SettingsSaved"

        DisplayMessageReceived ->
            "DisplayMessageReceived"

        WriterDataSaved ->
            "WriterDataSaved"


stringToInOperation : String -> InOperation
stringToInOperation operation =
    case operation of
        "ContentLoaded" ->
            ContentLoaded

        "TargetListReturned" ->
            TargetListReturned

        "AuthStateChanged" ->
            AuthStateChanged

        "SettingsLoaded" ->
            SettingsLoaded

        "SettingsSaved" ->
            SettingsSaved

        "DisplayMessageReceived" ->
            DisplayMessageReceived

        "WriterDataSaved" ->
            WriterDataSaved

        _ ->
            Unknown
