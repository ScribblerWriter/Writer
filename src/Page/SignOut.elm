module Page.SignOut exposing (Model, Msg, init, toSession)

import Appearance
import DisplayData exposing (DisplayData)
import Element exposing (..)
import Element.Background as Background
import Ports
import Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = NoOp



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    Session.mapLastUrl (\_ -> Nothing) session
        |> (\newSession ->
                ( { session = newSession }
                , Ports.sendJustMessage Ports.SignOut
                )
           )



-- VIEW


view : DisplayData msg
view =
    DisplayData.create "Signing out" body Nothing


body : Element msg
body =
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



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
