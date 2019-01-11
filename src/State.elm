module State exposing (State, decodeDimensions)

import Data.Target exposing (Target)
import Json.Decode as Decode


type alias State =
    { writtenCount : Int
    , windowDimensions : Dimensions
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


decodeDimensions : Decode.Value -> Dimensions
decodeDimensions value =
    case Decode.decodeValue dimensionDecoder value of
        Ok dimensions ->
            dimensions

        Err _ ->
            { width = 800, height = 600 }


dimensionDecoder : Decode.Decoder Dimensions
dimensionDecoder =
    Decode.map2 Dimensions
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
