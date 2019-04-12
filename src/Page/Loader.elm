module Page.Loader exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , toUser
    , update
    , view
    )

import Browser.Navigation as Nav
import DisplayData exposing (DisplayData)
import Element exposing (..)
import LocalState exposing (LocalState)
import Ports
import Route
import Session exposing (Session)
import Settings exposing (Settings)
import State exposing (State)
import User exposing (User)


type alias Model =
    { session : Session
    , user : User
    , settings : Maybe Settings
    , localState : Maybe LocalState
    , state : Maybe State
    }


type Msg
    = LoaderMsgReceived Ports.InMessage



-- INIT


init : User -> Session -> ( Model, Cmd Msg )
init user session =
    ( { user = user
      , session = session
      , settings = Nothing
      , localState = Nothing
      , state = Nothing
      }
    , Cmd.batch
        [ Ports.loadSettings (User.getUid user)
        , Ports.loadLocalSettings
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoaderMsgReceived message ->
            resolvePortMsg model message


resolvePortMsg : Model -> Ports.InMessage -> ( Model, Cmd Msg )
resolvePortMsg model msg =
    let
        _ =
            Debug.log "resolvePortMsg" msg.content
    in
    case Ports.stringToInOperation msg.operation of
        Ports.SettingsLoaded ->
            let
                _ =
                    Debug.log "Settings on their way in." msg.content
            in
            updateSettings model msg

        Ports.LocalStorageLoaded ->
            let
                _ =
                    Debug.log "Local settings on their way in." msg.content
            in
            updateLocalState model msg

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


updateLocalState : Model -> Ports.InMessage -> ( Model, Cmd Msg )
updateLocalState model msg =
    case msg.content of
        Just json ->
            { model | localState = Just (LocalState.decode json) }
                |> routeIfLoaded

        Nothing ->
            ( model, Cmd.none )


routeIfLoaded : Model -> ( Model, Cmd Msg )
routeIfLoaded model =
    case ( model.settings, model.localState ) of
        ( Just settings, Just localState ) ->
            let
                _ =
                    Debug.log "Settings loaded!" <| Session.getLastUrl model.session
            in
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



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.incomingMessage LoaderMsgReceived



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toUser : Model -> User
toUser model =
    model.user
