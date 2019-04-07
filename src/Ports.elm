port module Ports exposing
    ( InMessage
    , InOperation(..)
    , incomingMessage
    , loadLocalSettings
    , loadSettings
    , signIn
    , signOut
    , signUp
    , stringToInOperation
    )

import Credentials exposing (Credentials)
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
    = SaveLocalStorage
    | LoadLocalStorage
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
    | LocalStorageLoaded
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


loadLocalSettings : Cmd msg
loadLocalSettings =
    sendMessage LoadLocalStorage (Just LocalStorageLoaded) Nothing


signIn : Credentials -> Cmd msg
signIn creds =
    Just (Credentials.encode creds)
        |> sendMessage SignIn Nothing


signUp : Credentials -> Cmd msg
signUp creds =
    Just (Credentials.encode creds)
        |> sendMessage SignUp Nothing


signOut : Cmd msg
signOut =
    sendMessage SignOut Nothing Nothing



-- ENCODE MESSAGES


encodeLoadSettings : Uid -> Encode.Value
encodeLoadSettings uid =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "doc", Uid.encode uid )
        ]



-- PORT OPERATIONS


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
        SaveLocalStorage ->
            "SaveLocalStorage"

        LoadLocalStorage ->
            "LoadLocalStorage"

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
        LocalStorageLoaded ->
            "LocalStorageLoaded"

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
        "LocalStorageLoaded" ->
            LocalStorageLoaded

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
