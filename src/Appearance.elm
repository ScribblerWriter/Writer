module Appearance exposing
    ( progressBarBackground
    , progressBarForeground
    , siteActionButtonColor
    , siteBackgroundBlue
    , siteFocusStyle
    , siteFontSize
    , siteLightFontColor
    )

import Element exposing (..)
import Element.Font as Font


siteBackgroundBlue : Color
siteBackgroundBlue =
    rgb255 13 70 113


siteLightFontColor : Color
siteLightFontColor =
    rgb255 240 240 240


siteFocusStyle : Option
siteFocusStyle =
    Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }


siteFontSize : Int
siteFontSize =
    14


siteActionButtonColor : Color
siteActionButtonColor =
    rgb255 78 185 37


progressBarBackground : Color
progressBarBackground =
    rgb255 200 200 200


progressBarForeground : Color
progressBarForeground =
    rgb255 78 185 37
