module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Json.Decode as Decode
import Link
import Page.Loader as Loader
import Page.SignIn as SignIn
import Page.SignOut as SignOut
import Page.SignUp as SignUp
import PageFrame
import Ports
import Route exposing (Route)
import Session exposing (Session)
import State exposing (State)
import Url exposing (Url)
import User exposing (User)



-- MODEL


type Model
    = Redirect Session
    | SignIn SignIn.Model
    | SignUp SignUp.Model
    | SignOut SignOut.Model
    | Loader Loader.Model



-- INIT


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Session.create flags url key
        |> Redirect
        |> changeRouteTo (Route.fromUrl url)



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotSignInMsg SignIn.Msg
    | GotSignUpMsg SignUp.Msg
    | GotSignOutMsg SignOut.Msg
    | GotLoaderMsg Loader.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "main update" msg
    in
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( LinkClicked request, _ ) ->
            case request of
                Browser.Internal url ->
                    toSession model
                        |> Session.getKey
                        |> (\key -> ( model, Nav.pushUrl key (Url.toString url) ))

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( GotSignInMsg subMsg, SignIn subModel ) ->
            SignIn.update subMsg subModel
                |> updateWith SignIn GotSignInMsg model

        ( GotSignUpMsg subMsg, SignUp subModel ) ->
            SignUp.update subMsg subModel
                |> updateWith SignUp GotSignUpMsg model

        ( GotLoaderMsg subMsg, Loader subModel ) ->
            Loader.update subMsg subModel
                |> updateWith Loader GotLoaderMsg model

        _ ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Just Route.SignIn ->
            toSession model
                |> SignIn.init
                |> updateWith SignIn GotSignInMsg model

        Just Route.SignUp ->
            toSession model
                |> SignUp.init
                |> updateWith SignUp GotSignUpMsg model

        Just Route.SignOut ->
            toSession model
                |> SignOut.init
                |> updateWith SignOut GotSignOutMsg model

        Just Route.Loading ->
            routeWithUser Loader.init Loader GotLoaderMsg model

        Just Route.Failure ->
            ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


routeWithUser : (User -> Session -> ( subModel, Cmd subMsg )) -> (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( Model, Cmd Msg )
routeWithUser subInit toModel toMsg model =
    let
        session =
            toSession model
    in
    case toUser model of
        Just user ->
            subInit user session
                |> updateWith toModel toMsg model

        Nothing ->
            ( Session.getKey session, Link.getAbsolute Link.SignIn )
                |> (\( key, url ) -> ( model, Nav.pushUrl key url ))


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        SignIn subModel ->
            SignIn.toSession subModel

        SignUp subModel ->
            SignUp.toSession subModel

        SignOut subModel ->
            SignOut.toSession subModel

        Loader subModel ->
            Loader.toSession subModel


toUser : Model -> Maybe User
toUser model =
    case model of
        SignIn subModel ->
            SignIn.toUser subModel

        SignUp subModel ->
            SignUp.toUser subModel

        Loader subModel ->
            Just <| Loader.toUser subModel

        _ ->
            Nothing



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        SignIn subModel ->
            SignIn.view subModel
                |> PageFrame.view GotSignInMsg

        SignUp subModel ->
            SignUp.view subModel
                |> PageFrame.view GotSignUpMsg

        Loader subModel ->
            Loader.view subModel
                |> PageFrame.view GotLoaderMsg

        _ ->
            { title = "Egads!"
            , body = [ Html.text "You have found a non-existent page! WTF?" ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SignIn _ ->
            Sub.map GotSignInMsg SignIn.subscriptions

        SignUp _ ->
            Sub.map GotSignUpMsg SignUp.subscriptions

        Loader _ ->
            Sub.map GotLoaderMsg Loader.subscriptions

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
