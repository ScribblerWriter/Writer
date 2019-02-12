module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (text)
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Authenticator as Authenticator
import Page.Settings as Settings
import Page.SignerOuter as SignerOuter
import Page.TargetSelector as TargetSelector
import Page.Writer as Writer
import Ports
import Skeleton
import State exposing (State)
import Time
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { page : Page
    , state : State
    , returnPage : ReturnPage
    }


type Page
    = NotFound
    | Writer Writer.Model
    | TargetSelector TargetSelector.Model
    | Authenticator Authenticator.Model
    | SignerOuter
    | Settings Settings.Model


type ReturnPage
    = ToWriter
    | ToTargetSelector
    | ToSettings


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    stepUrl url
        { page = NotFound
        , returnPage = ToWriter
        , state =
            { additiveCount = 0
            , actualCount = 0
            , currentText = ""
            , currentTarget = Nothing
            , currentTargetTimerInSecs = 0
            , winProgress = 0
            , ended = State.No
            , countMethod = State.Additive
            , windowDimensions = State.decodeDimensions flags
            , user = Nothing
            , settings = Nothing
            , key = key
            }
        }
        |> (\( model, cmd ) ->
                ( model
                , Cmd.batch
                    [ cmd
                    , Ports.sendMessageWithJustResponse Ports.LoadContent Ports.ContentLoaded
                    ]
                )
           )



-- View


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            Skeleton.view model.state never Skeleton.noPageFound

        Writer writerModel ->
            Skeleton.view model.state GotWriterMsg (Writer.view writerModel model.state)

        TargetSelector targetSelectorModel ->
            Skeleton.view model.state GotTargetSelectorMsg (TargetSelector.view targetSelectorModel model.state)

        Authenticator authenticatorModel ->
            Skeleton.view model.state GotAuthenticatorMsg (Authenticator.view authenticatorModel)

        SignerOuter ->
            Skeleton.view model.state GotSignerOuterMsg SignerOuter.view

        Settings settingsModel ->
            Skeleton.view model.state GotSettingsMsg (Settings.view settingsModel model.state)



-- Update


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | PortMessageReceived Ports.InMessage
    | GotWriterMsg Writer.Msg
    | GotTargetSelectorMsg TargetSelector.Msg
    | GotAuthenticatorMsg Authenticator.Msg
    | GotSignerOuterMsg SignerOuter.Msg
    | GotSettingsMsg Settings.Msg
    | TargetTimerTicked Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            updateLinkClick urlRequest model

        UrlChanged url ->
            stepUrl url model

        PortMessageReceived msg ->
            updateMessageReceived msg model

        TargetTimerTicked _ ->
            ( { model | state = updateTargetTimer model.state }, Cmd.none )

        GotWriterMsg msg ->
            updateWriter msg model

        GotTargetSelectorMsg msg ->
            updateTargetSelector msg model

        GotAuthenticatorMsg msg ->
            updateAuthenticator msg model

        GotSignerOuterMsg msg ->
            updateSignerOuter msg model

        GotSettingsMsg msg ->
            updateSettings msg model


updateMessageReceived : Ports.InMessage -> Model -> ( Model, Cmd Msg )
updateMessageReceived message model =
    case Ports.stringToInOperation message.operation of
        Ports.ContentLoaded ->
            ( { model | state = State.decodeLoadedState message.content model.state }, Cmd.none )

        Ports.AuthStateChanged ->
            updateUser message.content model

        Ports.SettingsLoaded ->
            updateNewSettings message.content model

        _ ->
            ( model, Cmd.none )


updateUser : Maybe Decode.Value -> Model -> ( Model, Cmd Msg )
updateUser value model =
    case value of
        Nothing ->
            model.state
                |> (\state -> { state | user = Nothing })
                |> (\state ->
                        ( { model | state = state, returnPage = pageToReturnPage model.page }
                        , Nav.pushUrl model.state.key (Url.Builder.absolute [ "signin" ] [])
                        )
                   )

        Just user ->
            model.state
                |> (\state -> { state | user = State.decodeUser user })
                |> (\state ->
                        ( { model | state = state }
                        , Cmd.batch
                            [ Nav.pushUrl model.state.key (Url.Builder.absolute [ returnPageToUrlString model.returnPage ] [])
                            , encodeSettingsLoad state.user
                            ]
                        )
                   )


encodeSettingsLoad : Maybe State.User -> Cmd Msg
encodeSettingsLoad user =
    case user of
        Just user_ ->
            Ports.sendMessageWithContentAndResponse
                Ports.QueryDbSingle
                (Encode.object
                    [ ( "collection", Encode.string "users" )
                    , ( "doc", Encode.string user_.uid )
                    ]
                )
                Ports.SettingsLoaded

        Nothing ->
            Cmd.none


pageToReturnPage : Page -> ReturnPage
pageToReturnPage page =
    case page of
        TargetSelector _ ->
            ToTargetSelector

        Settings _ ->
            ToSettings

        Writer _ ->
            ToWriter

        Authenticator _ ->
            ToWriter

        SignerOuter ->
            ToWriter

        NotFound ->
            ToWriter


returnPageToUrlString : ReturnPage -> String
returnPageToUrlString page =
    case page of
        ToTargetSelector ->
            "target"

        ToSettings ->
            "settings"

        ToWriter ->
            ""


updateNewSettings : Maybe Decode.Value -> Model -> ( Model, Cmd Msg )
updateNewSettings value model =
    case value of
        Just settings ->
            model.state
                |> (\state -> { state | settings = State.decodeSettings settings })
                |> (\state -> ( { model | state = state }, Cmd.none ))

        Nothing ->
            ( model, Cmd.none )


updateTargetTimer : State -> State
updateTargetTimer state =
    case state.currentTarget of
        Nothing ->
            state

        Just target ->
            if target.new then
                { state
                    | currentTarget = Just { target | new = False }
                    , currentTargetTimerInSecs = target.minutes * 60
                    , winProgress = 0
                    , ended = State.No
                }

            else if state.currentTargetTimerInSecs <= 0 && state.ended /= State.WordsReached then
                { state | ended = State.TimeExpired }

            else if state.winProgress >= target.count then
                { state | ended = State.WordsReached }

            else
                { state | currentTargetTimerInSecs = state.currentTargetTimerInSecs - 1 }


updateLinkClick : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
updateLinkClick urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.state.key (Url.toString url)
                , updatePageLinkClick model
                ]
            )

        Browser.External href ->
            ( model
            , Nav.load href
            )


updatePageLinkClick : Model -> Cmd Msg
updatePageLinkClick model =
    case model.page of
        Writer writerModel ->
            Writer.updatePageLinkClick writerModel model.state

        TargetSelector targetSelectorModel ->
            Cmd.none

        Authenticator authenticatorModel ->
            Cmd.none

        SignerOuter ->
            Cmd.none

        Settings settingsModel ->
            Cmd.none

        NotFound ->
            Cmd.none


updateWriter : Writer.Msg -> Model -> ( Model, Cmd Msg )
updateWriter msg model =
    case model.page of
        Writer writerModel ->
            Writer.update msg writerModel model.state
                |> (\( state, data ) -> stepWriter { model | state = state } data)

        _ ->
            ( model, Cmd.none )


updateTargetSelector : TargetSelector.Msg -> Model -> ( Model, Cmd Msg )
updateTargetSelector msg model =
    case model.page of
        TargetSelector targetSelectorModel ->
            TargetSelector.update msg targetSelectorModel model.state
                |> (\( state, data ) -> stepTargetSelector { model | state = state } data)

        _ ->
            ( model, Cmd.none )


updateAuthenticator : Authenticator.Msg -> Model -> ( Model, Cmd Msg )
updateAuthenticator msg model =
    case model.page of
        Authenticator authenticatorModel ->
            Authenticator.update msg authenticatorModel
                |> (\data -> stepAuthenticator model data)

        _ ->
            ( model, Cmd.none )


updateSignerOuter : SignerOuter.Msg -> Model -> ( Model, Cmd Msg )
updateSignerOuter msg model =
    ( model, Cmd.none )


updateSettings : Settings.Msg -> Model -> ( Model, Cmd Msg )
updateSettings msg model =
    case model.page of
        Settings settingsModel ->
            Settings.update msg settingsModel model.state
                |> (\( state, data ) -> stepSettings { model | state = state } data)

        _ ->
            ( model, Cmd.none )



-- Routing


stepWriter : Model -> ( Writer.Model, Cmd Writer.Msg ) -> ( Model, Cmd Msg )
stepWriter model ( writerModel, writerCmds ) =
    ( { model | page = Writer writerModel }
    , Cmd.map GotWriterMsg writerCmds
    )


stepTargetSelector : Model -> ( TargetSelector.Model, Cmd TargetSelector.Msg ) -> ( Model, Cmd Msg )
stepTargetSelector model ( targetSelectorModel, targetSelectorCmds ) =
    ( { model | page = TargetSelector targetSelectorModel }
    , Cmd.map GotTargetSelectorMsg targetSelectorCmds
    )


stepAuthenticator : Model -> ( Authenticator.Model, Cmd Authenticator.Msg ) -> ( Model, Cmd Msg )
stepAuthenticator model ( authenticatorModel, authenticatorCmds ) =
    ( { model | page = Authenticator authenticatorModel }
    , Cmd.map GotAuthenticatorMsg authenticatorCmds
    )


stepSignerOuter : Model -> Cmd SignerOuter.Msg -> ( Model, Cmd Msg )
stepSignerOuter model signerOuterCmd =
    ( { model | page = SignerOuter }
    , Cmd.map GotSignerOuterMsg signerOuterCmd
    )


stepSettings : Model -> ( Settings.Model, Cmd Settings.Msg ) -> ( Model, Cmd Msg )
stepSettings model ( settingsModel, settingsCmds ) =
    ( { model | page = Settings settingsModel }
    , Cmd.map GotSettingsMsg settingsCmds
    )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route top (stepWriter model Writer.init)
                , route (s "target") (stepTargetSelector { model | returnPage = ToTargetSelector } TargetSelector.init)
                , route (s "signin") (stepAuthenticator model Authenticator.init)
                , route (s "signout") (stepSignerOuter model SignerOuter.init)
                , route (s "settings") (stepSettings { model | returnPage = ToSettings } Settings.init)
                ]
    in
    case Parser.parse parser url of
        Just result ->
            result

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    (case model.page of
        NotFound ->
            Sub.none

        Writer writerModel ->
            Sub.map GotWriterMsg (Writer.subscriptions writerModel)

        TargetSelector targetSelectorModel ->
            Sub.map GotTargetSelectorMsg (TargetSelector.subscriptions targetSelectorModel)

        Authenticator authenticatorModel ->
            Sub.map GotAuthenticatorMsg (Authenticator.subscriptions authenticatorModel)

        SignerOuter ->
            Sub.none

        Settings settingsModel ->
            Sub.map GotSettingsMsg (Settings.subscriptions settingsModel)
    )
        |> (\subs ->
                Sub.batch
                    [ subs
                    , Ports.incomingMessage PortMessageReceived
                    , Time.every 1000 TargetTimerTicked
                    ]
           )
