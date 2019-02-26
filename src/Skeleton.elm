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
            state
            (buildHeader state pageData.headerSettings)
            (Element.map pageMapper pageData.body)
        ]
    }


composePage : State -> Element msg -> Element msg -> Html msg
composePage state header body =
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
            , displayMessages state.messages
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


displayMessages : List State.Message -> Element msg
displayMessages messages =
    if List.isEmpty messages then
        none

    else
        column [ width fill ] <|
            List.map displaySingleMessage messages


displaySingleMessage : State.Message -> Element msg
displaySingleMessage message =
    row
        [ width fill
        , height <| px 20
        , Background.color <| messageBackgroundColor message.severity
        , Font.color Appearance.siteLightFontColor
        ]
        [ el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            ]
          <|
            text message.body
        ]


messageBackgroundColor : State.Severity -> Color
messageBackgroundColor severity =
    case severity of
        State.Error ->
            rgb255 150 10 10

        State.Warning ->
            rgb255 150 150 10

        State.Info ->
            Appearance.siteBackgroundDark


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
                            actionButton buttonSettings
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
                    text <|
                        state.settings.displayName
                , el
                    [ padding 10
                    , centerY
                    , alignRight
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    homeButton
                , el
                    [ padding 10
                    , centerY
                    , alignRight
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    settingsButton
                , el
                    [ padding 10
                    , centerY
                    , alignRight
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    signOutButton
                ]


actionButton : LinkSettings -> Element msg
actionButton settings =
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


signOutButton : Element msg
signOutButton =
    link
        [ centerY
        , alignRight
        , Font.color Appearance.siteLightFontColor
        ]
        { url = "/signout"
        , label = text "Sign Out"
        }


settingsButton : Element msg
settingsButton =
    link
        [ centerY
        , alignRight
        , Font.color Appearance.siteLightFontColor
        ]
        { url = "/settings"
        , label = text "Settings"
        }


homeButton : Element msg
homeButton =
    link
        [ centerY
        , alignRight
        , Font.color Appearance.siteLightFontColor
        ]
        { url = "/"
        , label = text "Home"
        }
