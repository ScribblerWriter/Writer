module Appearance exposing
    ( progressBarBackground
    , progressBarForeground
    , siteActionButtonColor
    , siteBackgroundDark
    , siteBackgroundLight
    , siteFocusStyle
    , siteFontSize
    , siteLightFontColor
    , siteTargetSelectionBackground
    )

import Element exposing (..)
import Element.Font as Font


siteBackgroundDark : Color
siteBackgroundDark =
    rgb255 13 70 113


siteLightFontColor : Color
siteLightFontColor =
    rgb255 240 240 240


siteBackgroundLight : Color
siteBackgroundLight =
    rgb255 255 255 255


siteTargetSelectionBackground : Color
siteTargetSelectionBackground =
    rgb255 108 160 229


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
