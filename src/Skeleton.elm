module Skeleton exposing (Msg, PageData, noPageFound, view)

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


type alias PageData msg =
    { title : String
    , body : Element msg
    }


type Msg
    = NoOp
    | TargetClicked
    | CancelTargetClicked
    | SignOutButtonClicked


type alias LinkSettings =
    { action : String
    , label : String
    }


type alias HeaderSettings =
    { writtenCount : Int
    , actionButtonSettings : Maybe LinkSettings
    , signOutButtonSettings : Maybe LinkSettings
    }


noPageFound : PageData msg
noPageFound =
    { title = "No page found!"
    , body = el [] <| text "No page found!"
    }


testHeaderSettings =
    { writtenCount = 543543
    , actionButtonSettings =
        Just { action = "/target", label = "TARGET" }
    , signOutButtonSettings =
        Just { action = "/signout", label = "Sign Out" }
    }


view : (a -> msg) -> PageData a -> Browser.Document msg
view pageMapper pageData =
    { title = pageData.title
    , body =
        [ composePage
            (buildHeader <| Just testHeaderSettings)
            (Element.map pageMapper pageData.body)
        ]
    }


composePage : Element msg -> Element msg -> Html msg
composePage header body =
    Element.layoutWith
        { options = [ Appearance.siteFocusStyle ] }
        [ Font.size Appearance.siteFontSize
        , Background.color Appearance.siteBackgroundBlue
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


buildHeader : Maybe HeaderSettings -> Element msg
buildHeader headerSettings =
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
                            ++ String.fromInt settings.writtenCount
                , el
                    [ padding 10
                    , centerY
                    , alignRight
                    , Font.color Appearance.siteLightFontColor
                    ]
                  <|
                    case settings.signOutButtonSettings of
                        Nothing ->
                            none

                        Just buttonSettings ->
                            buildSignOutButton buttonSettings
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


buildSignOutButton : LinkSettings -> Element msg
buildSignOutButton settings =
    link
        [ padding 10
        , centerY
        , alignRight
        , Font.color Appearance.siteLightFontColor
        ]
        { url = settings.action
        , label = text settings.label
        }



{-
   buildContent : Element msg
   buildContent =
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
               text "And here's the middle!"
-}
