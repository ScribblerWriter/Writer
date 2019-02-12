module Appearance exposing
    ( progressBarBackground
    , progressBarForeground
    , siteActionButtonColor
    , siteBackgroundDark
    , siteBackgroundLight
    , siteBackgroundTargetSelection
    , siteFocusStyle
    , siteFontSize
    , siteLightFontColor
    )

import Element exposing (..)
import Element.Font as Font


siteBackgroundDark : Color
siteBackgroundDark =
    rgb255 13 70 113


siteBackgroundLight : Color
siteBackgroundLight =
    rgb255 255 255 255


siteBackgroundTargetSelection : Color
siteBackgroundTargetSelection =
    rgb255 108 160 229


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
