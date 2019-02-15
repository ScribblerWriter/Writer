module Appearance exposing
    ( black
    , mediumGray
    , progressBarBackground
    , progressBarForeground
    , siteActionButtonColor
    , siteBackgroundDark
    , siteBackgroundLight
    , siteBackgroundMediumDark
    , siteBackgroundTargetSelection
    , siteFocusStyle
    , siteFontSize
    , siteHeaderSize
    , siteLightFontColor
    )

import Element exposing (..)
import Element.Font as Font


siteBackgroundDark : Color
siteBackgroundDark =
    rgb255 13 70 113


siteBackgroundMediumDark : Color
siteBackgroundMediumDark =
    rgb255 23 80 123


siteBackgroundLight : Color
siteBackgroundLight =
    rgb255 255 255 255


siteBackgroundTargetSelection : Color
siteBackgroundTargetSelection =
    rgb255 108 160 229


siteLightFontColor : Color
siteLightFontColor =
    rgb255 240 240 240


mediumGray : Color
mediumGray =
    rgb255 110 110 110


black : Color
black =
    rgb255 0 0 0


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


siteHeaderSize : Int
siteHeaderSize =
    18


siteActionButtonColor : Color
siteActionButtonColor =
    rgb255 78 160 37


progressBarBackground : Color
progressBarBackground =
    rgb255 200 200 200


progressBarForeground : Color
progressBarForeground =
    rgb255 78 185 37
