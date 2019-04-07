port module Ports exposing
    ( InMessage
    , InOperation(..)
    , OutOperation(..)
    , incomingMessage
    , loadSettings
    , sendJustMessage
    , sendMessageWithContentAndResponse
    , sendMessageWithJustContent
    , sendMessageWithJustResponse
    , stringToInOperation
    )

import Json.Encode as Encode
import Uid exposing (Uid)


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
    | QueryDbSingleSubCollection
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
    | AuthMsgReceived
    | WriterDataSaved
    | WordCountLoaded



-- BUILD COMMANDS


loadSettings : Uid -> Cmd msg
loadSettings uid =
    Just (encodeLoadSettings uid)
        |> sendMessage QueryDbSingle (Just SettingsLoaded)



-- ENCODE MESSAGES


encodeLoadSettings : Uid -> Encode.Value
encodeLoadSettings uid =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "doc", Uid.encode uid )
        ]



-- PORT OPERATIONS


sendJustMessage : OutOperation -> Cmd msg
sendJustMessage operation =
    sendMessage operation Nothing Nothing


sendMessageWithJustContent : OutOperation -> Encode.Value -> Cmd msg
sendMessageWithJustContent operation content =
    sendMessage operation Nothing (Just content)


sendMessageWithJustResponse : OutOperation -> InOperation -> Cmd msg
sendMessageWithJustResponse operation returnOperation =
    sendMessage operation (Just returnOperation) Nothing


sendMessageWithContentAndResponse : OutOperation -> InOperation -> Encode.Value -> Cmd msg
sendMessageWithContentAndResponse operation returnOperation content =
    sendMessage operation (Just returnOperation) (Just content)


sendMessage : OutOperation -> Maybe InOperation -> Maybe Encode.Value -> Cmd msg
sendMessage operation returnOperation content =
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



-- CONVERSIONS


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

        QueryDbSingleSubCollection ->
            "QueryDbSingleSubCollection"

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

        AuthMsgReceived ->
            "AuthMsgReceived"

        WriterDataSaved ->
            "WriterDataSaved"

        WordCountLoaded ->
            "WordCountLoaded"


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

        "AuthMsgReceived" ->
            AuthMsgReceived

        "WriterDataSaved" ->
            WriterDataSaved

        "WordCountLoaded" ->
            WordCountLoaded

        _ ->
            Unknown
