module Page.Loader exposing (Model, Msg, init, toSession, toUser, update, view)

import Browser.Navigation as Nav
import DisplayData exposing (DisplayData)
import Element exposing (..)
import Ports
import Route
import Session exposing (Session)
import Settings exposing (Settings)
import User exposing (User)


type alias Model =
    { session : Session
    , user : User
    , settings : Maybe Settings
    }


type Msg
    = GotPortMsg Ports.InMessage



-- INIT


init : User -> Session -> ( Model, Cmd Msg )
init user session =
    ( { user = user
      , session = session
      , settings = Nothing
      }
    , Ports.loadSettings (User.getUid user)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPortMsg message ->
            resolvePortMsg model message


resolvePortMsg : Model -> Ports.InMessage -> ( Model, Cmd Msg )
resolvePortMsg model msg =
    case Ports.stringToInOperation msg.operation of
        Ports.SettingsLoaded ->
            updateSettings model msg

        Ports.ContentLoaded ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateSettings : Model -> Ports.InMessage -> ( Model, Cmd Msg )
updateSettings model msg =
    case msg.content of
        Just json ->
            { model | settings = Just (Settings.decode json) }
                |> routeIfLoaded

        Nothing ->
            ( model, Cmd.none )


routeIfLoaded : Model -> ( Model, Cmd Msg )
routeIfLoaded model =
    case model.settings of
        Just settings ->
            Session.getLastUrl model.session
                |> Route.toAbsolute
                |> Nav.pushUrl (Session.getKey model.session)
                |> (\cmd -> ( model, cmd ))

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> DisplayData msg
view model =
    DisplayData.create "Loading" (body model) Nothing


body : Model -> Element msg
body model =
    el
        [ width shrink
        , width shrink
        , spacing 10
        , centerX
        , centerY
        ]
    <|
        text "This is the amazing loading page!"



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toUser : Model -> User
toUser model =
    model.user
