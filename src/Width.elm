module Width exposing (Width, create, decode, getWidth, mapWidth)

import Json.Decode as Decode


type Width
    = Width Int


create : Int -> Width
create width =
    Width width



-- accessors


getWidth : Width -> Int
getWidth (Width width) =
    width


mapWidth : (Int -> Int) -> Width -> Width
mapWidth mapper width =
    getWidth width
        |> mapper
        |> Width



-- decoding


decode : Decode.Value -> Width
decode json =
    Decode.decodeValue widthDecoder json
        |> Result.withDefault 800
        |> Width


widthDecoder : Decode.Decoder Int
widthDecoder =
    Decode.field "width" Decode.int
