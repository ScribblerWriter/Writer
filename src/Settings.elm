module Settings exposing
    ( CountMethod(..)
    , Settings
    , decode
    , getCountMethod
    , getDisplayName
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional)


type Settings
    = Settings
        { displayName : String
        , countMethod : CountMethod
        }


type CountMethod
    = Additive
    | Subtractive



-- ACCESSORS


getDisplayName : Settings -> String
getDisplayName (Settings { displayName }) =
    displayName


getCountMethod : Settings -> CountMethod
getCountMethod (Settings { countMethod }) =
    countMethod



-- DECODING


decode : Decode.Value -> Settings
decode value =
    case Decode.decodeValue settingsDecoder value of
        Ok settings ->
            settings

        Err _ ->
            Settings { displayName = "", countMethod = Additive }


settingsDecoder : Decode.Decoder Settings
settingsDecoder =
    Decode.succeed
        (\displayName countMethod ->
            Settings
                { displayName = displayName
                , countMethod = countMethod
                }
        )
        |> optional "displayName" Decode.string ""
        |> optional "countMethod" methodDecoder Additive


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
