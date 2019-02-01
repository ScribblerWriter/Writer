module Skeleton exposing (HeaderSettings, LinkSettings, PageData, noPageFound, view)

-- This module has the skeleton/framework of the basic site structure,
-- handling things like standard headers, borders and so on. All pages
-- are passed in to be rendered within it, with their specific
-- requirements.
-- The top menu requires some additional settings to handle interactions
-- with it, including the targeting button and user settings and such.
-- The top menu may get its own module at some point if necessary.

import Appearance
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import State exposing (State)


type alias PageData msg =
    { title : String
    , body : Element msg
    , headerSettings : Maybe HeaderSettings
    }


type alias HeaderSettings =
    { actionButtonSettings : Maybe LinkSettings }


type alias LinkSettings =
    { action : String
    , label : String
    }


noPageFound : PageData msg
noPageFound =
    { title = "No page found!"
    , headerSettings = Nothing
    , body =
        el
            [ height fill
            , width fill
            , Background.color Appearance.siteLightFontColor
            ]
        <|
            el
                [ height shrink
                , width shrink
                , centerX
                , centerY
                ]
            <|
                text "No page found here!"
    }


view : State -> (a -> msg) -> PageData a -> Browser.Document msg
view state pageMapper pageData =
    { title = pageData.title
    , body =
        [ composePage
            (buildHeader state pageData.headerSettings)
            (Element.map pageMapper pageData.body)
        ]
    }


composePage : Element msg -> Element msg -> Html msg
composePage header body =
    Element.layoutWith
        { options = [ Appearance.siteFocusStyle ] }
        [ Font.size Appearance.siteFontSize
        , Background.color Appearance.siteBackgroundDark
        , width fill
        , height fill
        ]
    <|
        column
            [ width fill
            , height fill
            ]
            [ verticalSpacer
            , header
            , row
                [ width fill
                , height fill
                , scrollbars
                ]
                [ horizontalSpacer
                , body
                , horizontalSpacer
                ]
            , verticalSpacer
            ]


verticalSpacer : Element msg
verticalSpacer =
    el
        [ width fill
        , height <| px 5
        ]
        none


horizontalSpacer : Element msg
horizontalSpacer =
    el
        [ height fill
        , width <| px 5
        ]
        none


buildHeader : State -> Maybe HeaderSettings -> Element msg
buildHeader state headerSettings =
    case headerSettings of
        Nothing ->
            none

        Just settings ->
            row
                [ width fill
                , height <| px 50
                , inFront <|
                    case settings.actionButtonSettings of
                        Nothing ->
                            none

                        Just buttonSettings ->
                            buildActionButton buttonSettings
                ]
                [ el
                    [ padding 10
                    , centerY
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    text <|
                        "Written so far: "
                            ++ String.fromInt state.additiveCount
                , el
                    [ padding 10
                    , centerY
                    , alignRight
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    buildSignOutButton
                ]


buildActionButton : LinkSettings -> Element msg
buildActionButton settings =
    link
        [ centerX
        , centerY
        , padding 2
        , Border.width 2
        , Border.rounded 2
        , Background.color Appearance.siteActionButtonColor
        , Font.color Appearance.siteLightFontColor
        ]
        { url = settings.action
        , label = text settings.label
        }


buildSignOutButton : Element msg
buildSignOutButton =
    link
        [ padding 10
        , centerY
        , alignRight
        , Font.color Appearance.siteLightFontColor
        ]
        { url = "/signout"
        , label = text "Sign Out"
        }
