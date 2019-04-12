module LocalState exposing
    ( LocalState
    , decode
    , getActualCount
    , getAdditiveCount
    , getCurrentText
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional)


type LocalState
    = LocalState
        { additiveCount : Int
        , actualCount : Int
        , currentText : String
        }



-- ACCESSORS


getAdditiveCount : LocalState -> Int
getAdditiveCount (LocalState { additiveCount }) =
    additiveCount


getActualCount : LocalState -> Int
getActualCount (LocalState { actualCount }) =
    actualCount


getCurrentText : LocalState -> String
getCurrentText (LocalState { currentText }) =
    currentText



-- DECODING


decode : Decode.Value -> LocalState
decode json =
    case Decode.decodeValue localStateDecoder json of
        Ok localState ->
            localState

        Err _ ->
            LocalState
                { additiveCount = 0
                , actualCount = 0
                , currentText = ""
                }


localStateDecoder : Decode.Decoder LocalState
localStateDecoder =
    Decode.succeed
        (\additiveCount actualCount currentText ->
            LocalState
                { additiveCount = additiveCount
                , actualCount = actualCount
                , currentText = currentText
                }
        )
        |> optional "count" Decode.int 0
        |> optional "actualCount" Decode.int 0
        |> optional "text" Decode.string ""
