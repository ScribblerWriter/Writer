module DisplayData exposing
    ( DisplayData
    , create
    , getBody
    , getHeader
    , getTitle
    )

import ActionButton exposing (ActionButton)
import Element exposing (..)
import Header exposing (Header)


type DisplayData msg
    = DisplayData
        { title : String
        , body : Element msg
        , header : Maybe Header
        }


create : String -> Element msg -> Maybe Header -> DisplayData msg
create title body header =
    DisplayData
        { title = title
        , body = body
        , header = header
        }



-- accessors


getTitle : DisplayData msg -> String
getTitle (DisplayData { title }) =
    title


getBody : DisplayData msg -> Element msg
getBody (DisplayData { body }) =
    body


getHeader : DisplayData msg -> Maybe Header
getHeader (DisplayData { header }) =
    header
