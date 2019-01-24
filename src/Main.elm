module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (text)
import Json.Decode as Decode
import Json.Encode as Encode
import Page.TargetSelector as TargetSelector
import Page.Writer as Writer
import Skeleton
import State exposing (State)
import Url
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
    }


type Page
    = NotFound
    | Writer Writer.Model
    | TargetSelector TargetSelector.Model


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    stepUrl url
        { page = NotFound
        , state =
            { writtenCount = 0
            , windowDimensions = State.decodeDimensions flags
            , currentTarget = Nothing
            , key = key
            }
        }



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



-- Update


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotWriterMsg Writer.Msg
    | GotTargetSelectorMsg TargetSelector.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            updateLinkClick urlRequest model

        UrlChanged url ->
            stepUrl url model

        GotWriterMsg msg ->
            updateWriter msg model

        GotTargetSelectorMsg msg ->
            updateTargetSelector msg model


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

        NotFound ->
            Cmd.none


updateWriter : Writer.Msg -> Model -> ( Model, Cmd Msg )
updateWriter msg model =
    let
        _ =
            Debug.log "updateWriter:" msg
    in
    case model.page of
        Writer writerModel ->
            Writer.update msg writerModel model.state
                |> (\( state, data ) -> stepWriter { model | state = state } data)

        _ ->
            ( model, Cmd.none )


updateTargetSelector : TargetSelector.Msg -> Model -> ( Model, Cmd Msg )
updateTargetSelector msg model =
    let
        _ =
            Debug.log "updateTargetSelector:" msg
    in
    case model.page of
        TargetSelector targetSelectorModel ->
            TargetSelector.update msg targetSelectorModel model.state
                |> (\( state, data ) -> stepTargetSelector { model | state = state } data)

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


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route top (stepWriter model Writer.init)
                , route (s "target") (stepTargetSelector model TargetSelector.init)
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
    case model.page of
        NotFound ->
            Sub.none

        Writer writerModel ->
            Sub.map GotWriterMsg (Writer.subscriptions writerModel)

        TargetSelector targetSelectorModel ->
            Sub.map GotTargetSelectorMsg (TargetSelector.subscriptions targetSelectorModel)
