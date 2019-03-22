module Dimensions exposing
    ( Dimensions
    , create
    , decode
    , getHeight
    , getWidth
    , mapHeight
    , mapWidth
    )

import Height exposing (Height)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Width exposing (Width)


type Dimensions
    = Dimensions
        { width : Width
        , height : Height
        }


create : Width -> Height -> Dimensions
create width height =
    Dimensions { width = width, height = height }



-- accessors


getWidth : Dimensions -> Width
getWidth (Dimensions { width }) =
    width


mapWidth : (Width -> Width) -> Dimensions -> Dimensions
mapWidth mapper (Dimensions { width, height }) =
    mapper width
        |> (\newWidth -> Dimensions { width = newWidth, height = height })


getHeight : Dimensions -> Height
getHeight (Dimensions { height }) =
    height


mapHeight : (Height -> Height) -> Dimensions -> Dimensions
mapHeight mapper (Dimensions { width, height }) =
    mapper height
        |> (\newHeight -> Dimensions { width = width, height = newHeight })



-- decoding


decode : Decode.Value -> Dimensions
decode json =
    let
        width =
            Width.decode json

        height =
            Height.decode json
    in
    Dimensions { width = width, height = height }
