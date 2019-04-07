module State exposing
    ( CountMethod(..)
    , Ended(..)
    , Settings
    , State
    , User
    , decodeDimensions
    , decodeLoadedState
    , decodeSettings
    , decodeUser
    , defaultSettings
    , encodeSaveState
    , endReasonToString
    , methodEncoder
    )

import Browser.Navigation as Nav
import Data.Target exposing (Target)
import DisplayMessage
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Task
import Time


type alias State =
    { additiveCount : Int
    , actualCount : Int
    , currentText : String
    , currentTarget : Maybe Target
    , currentTargetTimerInSecs : Int
    , winProgress : Int
    , ended : Ended
    , windowDimensions : Dimensions
    , user : Maybe User
    , settings : Settings
    , messages : List DisplayMessage.Message
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , key : Nav.Key
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias User =
    { email : String
    , uid : String
    }


type alias Settings =
    { displayName : String
    , countMethod : CountMethod
    }


type CountMethod
    = Additive
    | Subtractive


type Ended
    = No
    | TimeExpired
    | WordsReached



-- Conversion


endReasonToString : Ended -> String
endReasonToString ended =
    case ended of
        No ->
            ""

        TimeExpired ->
            "Time's up!"

        WordsReached ->
            "You win!"



-- Decoders


decodeDimensions : Decode.Value -> Dimensions
decodeDimensions value =
    case Decode.decodeValue dimensionDecoder value of
        Ok dimensions ->
            dimensions

        Err _ ->
            { width = 800, height = 600 }


dimensionDecoder : Decode.Decoder Dimensions
dimensionDecoder =
    Decode.succeed Dimensions
        |> required "width" Decode.int
        |> required "height" Decode.int


decodeUser : Decode.Value -> Maybe User
decodeUser value =
    case Decode.decodeValue userDecoder value of
        Ok user ->
            Just user

        Err _ ->
            Nothing


userDecoder : Decode.Decoder User
userDecoder =
    Decode.succeed User
        |> required "email" Decode.string
        |> required "uid" Decode.string


decodeLoadedState : Maybe Decode.Value -> State -> State
decodeLoadedState content state =
    case content of
        Just data ->
            { state
                | additiveCount = getValue wordCountDecoder data 0
                , currentText = getValue textDecoder data ""
                , actualCount = getValue actualCountDecoder data 0
            }

        Nothing ->
            state


decodeSettings : Decode.Value -> Settings
decodeSettings value =
    case Decode.decodeValue settingsDecoder value of
        Ok settings ->
            settings

        Err _ ->
            defaultSettings


settingsDecoder : Decode.Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> optional "displayName" Decode.string defaultSettings.displayName
        |> optional "countMethod" methodDecoder defaultSettings.countMethod


defaultSettings : Settings
defaultSettings =
    { displayName = ""
    , countMethod = Additive
    }


getValue : Decode.Decoder a -> Decode.Value -> a -> a
getValue decoder string errorVal =
    case Decode.decodeValue decoder string of
        Err _ ->
            errorVal

        Ok value ->
            value


wordCountDecoder : Decode.Decoder Int
wordCountDecoder =
    Decode.field "count" Decode.int


textDecoder : Decode.Decoder String
textDecoder =
    Decode.field "text" Decode.string


actualCountDecoder : Decode.Decoder Int
actualCountDecoder =
    Decode.field "actualCount" Decode.int


methodDecoder : Decode.Decoder CountMethod
methodDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "additive" ->
                        Decode.succeed Additive

                    "subtractive" ->
                        Decode.succeed Subtractive

                    wrongValue ->
                        Decode.fail ("Count method decoding failed. Value: " ++ wrongValue)
            )



-- encoding


encodeSaveState : State -> Encode.Value
encodeSaveState state =
    Encode.object
        [ ( "count", Encode.int state.additiveCount )
        , ( "text", Encode.string state.currentText )
        , ( "actualCount", Encode.int state.actualCount )
        , ( "timestamp", Encode.int <| Time.posixToMillis state.currentTime )
        ]


methodEncoder : CountMethod -> Encode.Value
methodEncoder method =
    case method of
        Additive ->
            Encode.string "additive"

        Subtractive ->
            Encode.string "subtractive"
