module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (text)
import Page.Writer as Writer
import Skeleton
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


main : Program () Model Msg
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
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | Writer Writer.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , page = NotFound
        }



-- View


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            Skeleton.view never Skeleton.noPageFound

        Writer writerModel ->
            Skeleton.view GotWriterMsg (Writer.view writerModel)



-- Update


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotWriterMsg Writer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        GotWriterMsg writer ->
            ( model, Cmd.none )



-- Routing


stepWriter : Model -> ( Writer.Model, Cmd Writer.Msg ) -> ( Model, Cmd Msg )
stepWriter model ( writerModel, writerCmds ) =
    ( { model | page = Writer writerModel }
    , Cmd.none
    )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route top
                    (stepWriter model Writer.init)
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
subscriptions _ =
    Sub.none
