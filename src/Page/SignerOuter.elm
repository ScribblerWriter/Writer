module Page.SignerOuter exposing (Msg, init, view)

import Appearance
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Ports
import Skeleton


type Msg
    = NoOp


init : Cmd Msg
init =
    Ports.sendJustMessage Ports.SignOut


view : Skeleton.PageData Msg
view =
    { title = "Sign Out"
    , headerSettings = Nothing
    , body =
        el
            [ width fill
            , height fill
            , Background.color Appearance.siteBackgroundLight
            ]
        <|
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                ]
            <|
                text "Signing you out."
    }
