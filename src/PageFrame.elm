module PageFrame exposing (view)

import Appearance
import Browser exposing (Document)
import DisplayData exposing (DisplayData)
import Element as Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


view : (a -> msg) -> DisplayData a -> Document msg
view pageMapper displayData =
    { title = DisplayData.getTitle displayData
    , body =
        [ DisplayData.getBody displayData
            |> Element.map pageMapper
            |> buildPage
        ]
    }


buildPage : Element msg -> Html msg
buildPage body =
    Element.layoutWith
        { options = [ Appearance.siteFocusStyle ] }
        [ Font.size Appearance.siteFontSize
        , Background.color Appearance.siteBackgroundDark
        , width fill
        , height fill
        ]
    <|
        body
