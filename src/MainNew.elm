module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Error exposing (Error)
import Html
import Json.Decode as Decode
import Link
import Page.Loader as Loader
import Page.SignIn as SignIn
import Page.SignUp as SignUp
import PageFrame
import Ports
import Session exposing (Session)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)
import User exposing (User)


type Model
    = Unauthenticated UnauthPage Session
    | Loading Loader.Model Session
    | Running User Session
    | Failed Error Session
    | Redirect Session


type UnauthPage
    = SignIn SignIn.Model
    | SignUp SignUp.Model



-- init


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Session.create flags url key
        |> Redirect
        |> changeRouteTo url



-- update


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | PortMessageReceived Ports.InMessage
    | GotSignInMsg SignIn.Msg
    | GotSignUpMsg SignUp.Msg
    | GotLoaderMsg Loader.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            changeRouteTo url model

        LinkClicked request ->
            updateLinkClick request model

        PortMessageReceived portMsg ->
            resolvePortMessage model portMsg

        GotSignInMsg subMsg ->
            updateSignIn subMsg model

        GotSignUpMsg subMsg ->
            updateSignUp subMsg model

        GotLoaderMsg subMsg ->
            updateLoading subMsg model


updateLinkClick : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
updateLinkClick request model =
    case request of
        Browser.Internal url ->
            toSession model
                |> Session.getKey
                |> (\key -> ( model, Nav.pushUrl key (Url.toString url) ))

        Browser.External href ->
            ( model, Nav.load href )


resolvePortMessage : Model -> Ports.InMessage -> ( Model, Cmd Msg )
resolvePortMessage model msg =
    case Ports.stringToInOperation msg.operation of
        Ports.AuthStateChanged ->
            loadSite msg.content model

        _ ->
            ( model, Cmd.none )


loadSite : Maybe Decode.Value -> Model -> ( Model, Cmd Msg )
loadSite maybeUser model =
    case maybeUser of
        Nothing ->
            ( Session.getKey (toSession model)
            , Link.getUrl Link.SignIn
            )
                |> (\( key, url ) -> Nav.pushUrl key (Url.Builder.absolute [ url ] []))
                |> (\cmd -> ( model, cmd ))

        Just jsonUser ->
            case User.decode jsonUser of
                Ok user ->
                    toSession Session
                        |> Session.mapAuth (Session.Auth user)
                        |> (\session ->
                                ()
                           )
                            routeToLoader
                            (toSession model)
                            user

                Err error ->
                    routeToFailed (toSession model) error


updateSignIn : SignIn.Msg -> Model -> ( Model, Cmd Msg )
updateSignIn msg model =
    case model of
        Unauthenticated (SignIn subModel) session ->
            SignIn.update msg subModel
                |> routeToSignIn session

        _ ->
            ( model, Cmd.none )


updateSignUp : SignUp.Msg -> Model -> ( Model, Cmd Msg )
updateSignUp msg model =
    case model of
        Unauthenticated (SignUp subModel) session ->
            SignUp.update msg subModel
                |> routeToSignUp session

        _ ->
            ( model, Cmd.none )


updateLoading : Loader.Msg -> Model -> ( Model, Cmd Msg )
updateLoading msg model =
    case model of
        Loading subModel user session ->
            Loader.update msg subModel
                |> routeToLoader session user

        _ ->
            ( model, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    case model of
        Unauthenticated (SignIn subModel) session ->
            SignIn.view subModel
                |> PageFrame.view GotSignInMsg

        Unauthenticated (SignUp subModel) session ->
            SignUp.view subModel
                |> PageFrame.view GotSignUpMsg

        _ ->
            { title = "Egads!"
            , body = [ Html.text "You have found a non-existent page! WTF?" ]
            }



-- routing


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        parser =
            let
                session =
                    toSession model
            in
            oneOf
                [ route top (routeToSignIn session SignIn.init)
                , route (s "signin") (routeToSignIn session SignIn.init)
                , route (s "signup") (routeToSignUp session SignUp.init)
                ]
    in
    case Parser.parse parser url of
        Just result ->
            result

        Nothing ->
            Error.create "Unable to route to target url." ("Error url: " ++ Url.toString url)
                |> (\error -> ( Failed error (toSession model), Cmd.none ))


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


routeToSignIn : Session -> ( SignIn.Model, Cmd SignIn.Msg ) -> ( Model, Cmd Msg )
routeToSignIn session ( subModel, subMsg ) =
    ( Unauthenticated (SignIn subModel) session
    , Cmd.map GotSignInMsg subMsg
    )


routeToSignUp : Session -> ( SignUp.Model, Cmd SignUp.Msg ) -> ( Model, Cmd Msg )
routeToSignUp session ( subModel, subMsg ) =
    ( Unauthenticated (SignUp subModel) session
    , Cmd.map GotSignUpMsg subMsg
    )


routeToLoader : Session -> User -> ( Loader.Model, Cmd Loader.Msg ) -> ( Model, Cmd Msg )
routeToLoader session user ( subModel, subMsg ) =
    ( Loading subModel user session
    , Cmd.map GotLoaderMsg subMsg
    )


routeToFailed : Session -> Error -> ( Model, Cmd Msg )
routeToFailed session error =
    ( Failed error session, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        Unauthenticated _ session ->
            session

        Loading _ _ session ->
            session

        Running _ session ->
            session

        Failed _ session ->
            session

        Redirect session ->
            session



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model
        , Ports.incomingMessage PortMessageReceived
        ]


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model of
        Unauthenticated (SignIn _) _ ->
            Sub.map GotSignInMsg SignIn.subscriptions

        Unauthenticated (SignUp _) _ ->
            Sub.map GotSignUpMsg SignUp.subscriptions

        _ ->
            Sub.none



-- MAIN


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
