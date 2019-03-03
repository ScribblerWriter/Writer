module DisplayMessage exposing
    ( Code(..)
    , Message
    , Severity(..)
    , Source(..)
    , decodeDisplayMessage
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type alias Message =
    { body : String
    , severity : Severity
    , source : Source
    , code : Code
    }


type Source
    = Db
    | Auth
    | Internal


type Severity
    = Info
    | Warning
    | Error


type Code
    = Unknown
    | BadUser
    | BadPass
    | InvalidEmail
    | ParseError
    | WeakPassword
    | EmailAlreadyInUse
    | NoMessage



-- decoding


decodeDisplayMessage : Decode.Value -> Message
decodeDisplayMessage message =
    case Decode.decodeValue messageDecoder message of
        Ok message_ ->
            message_

        Err _ ->
            { body = "Error parsing messages to display"
            , severity = Warning
            , source = Internal
            , code = ParseError
            }


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.succeed Message
        |> required "body" Decode.string
        |> required "severity" severityDecoder
        |> required "source" sourceDecoder
        |> required "code" codeDecoder


severityDecoder : Decode.Decoder Severity
severityDecoder =
    Decode.string
        |> Decode.andThen
            (\severity -> Decode.succeed <| severityFromString severity)


severityFromString : String -> Severity
severityFromString severity =
    case severity of
        "error" ->
            Error

        "warning" ->
            Warning

        _ ->
            Info


sourceDecoder : Decode.Decoder Source
sourceDecoder =
    Decode.string
        |> Decode.andThen
            (\source -> Decode.succeed <| sourceFromString source)


sourceFromString : String -> Source
sourceFromString source =
    case source of
        "db" ->
            Db

        "auth" ->
            Auth

        _ ->
            Internal


codeDecoder : Decode.Decoder Code
codeDecoder =
    Decode.string
        |> Decode.andThen
            (\code -> Decode.succeed <| codeFromString code)


codeFromString : String -> Code
codeFromString code =
    case code of
        "bad-user" ->
            BadUser

        "bad-pass" ->
            BadPass

        "invalid-email" ->
            InvalidEmail

        "weak-password" ->
            WeakPassword

        "email-already-in-use" ->
            EmailAlreadyInUse

        _ ->
            Unknown
