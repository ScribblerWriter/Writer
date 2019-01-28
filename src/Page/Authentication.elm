module Page.Authentication exposing (Model, Msg, init, subscriptions, update, view)

import Appearance
import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Encode as Encode
import Skeleton
import State exposing (State)


type alias Model =
    { email : String
    , password : String
    }


type InputType
    = UserName
    | Password


type Msg
    = LoginInputReceived InputType String
    | LoginButtonClicked
    | SignUpButtonClicked
    | SignOutButtonClicked



-- init


init : ( Model, Cmd Msg )
init =
    ( { email = "", password = "" }, Cmd.none )



-- update


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        LoginInputReceived inputType string ->
            ( state, ( model, Cmd.none ) )

        LoginButtonClicked ->
            ( state, ( model, Cmd.none ) )

        SignUpButtonClicked ->
            ( state, ( model, Cmd.none ) )

        SignOutButtonClicked ->
            ( state, ( model, Cmd.none ) )



-- View


view : Model -> State -> Skeleton.PageData Msg
view model state =
    { title = "Login"
    , headerSettings = Nothing
    , body = showBody model state
    }


showBody : Model -> State -> Element Msg
showBody model state =
    row
        [ width shrink
        , height shrink
        , centerX
        , centerY
        ]
        [ column
            [ width fill
            , padding 10
            ]
            [ Input.email
                [ Input.focusedOnLoad ]
                { onChange = LoginInputReceived UserName
                , text = model.email
                , placeholder = Just <| Input.placeholder [] (text "Email address")
                , label = Input.labelHidden "Email address"
                }
            , Input.button
                loginPageButtonAttributes
                { onPress = Just LoginButtonClicked
                , label = text "Log in"
                }
            ]
        , column
            [ width fill
            , padding 10
            ]
            [ Input.newPassword []
                { onChange = LoginInputReceived Password
                , text = model.password
                , placeholder = Just <| Input.placeholder [] (text "Password")
                , label = Input.labelHidden "Password"
                , show = False
                }
            , Input.button
                loginPageButtonAttributes
                { onPress = Just SignUpButtonClicked
                , label = text "Sign up"
                }
            ]
        ]


loginPageButtonAttributes : List (Attribute msg)
loginPageButtonAttributes =
    [ centerX
    , height shrink
    , padding 5
    , Border.width 2
    , Border.rounded 5
    , Background.color Appearance.siteBackgroundBlue
    , Font.color Appearance.siteLightFontColor
    ]



-- encoding / Decoding


emailPassEncoder : String -> String -> Encode.Value
emailPassEncoder email pass =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "pass", Encode.string pass )
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
