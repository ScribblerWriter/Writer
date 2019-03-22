module Height exposing (Height, create, decode, getHeight, mapHeight)

import Json.Decode as Decode


type Height
    = Height Int


create : Int -> Height
create height =
    Height height



-- accessors


getHeight : Height -> Int
getHeight (Height height) =
    height


mapHeight : (Int -> Int) -> Height -> Height
mapHeight mapper height =
    getHeight height
        |> mapper
        |> Height



-- decoding


decode : Decode.Value -> Height
decode json =
    Decode.decodeValue heightDecoder json
        |> Result.withDefault 600
        |> Height


heightDecoder : Decode.Decoder Int
heightDecoder =
    Decode.field "height" Decode.int
