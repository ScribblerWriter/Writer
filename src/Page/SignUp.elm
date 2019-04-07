module Page.SignUp exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , toUser
    , update
    , view
    )

import Appearance
import Browser.Navigation as Nav
import Credentials
import CustomHtmlEvents
import DisplayData exposing (DisplayData)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Email
import Html.Attributes
import Json.Decode as Decode
import Link exposing (Destination(..))
import Password
import Ports
import Route
import Session exposing (Session)
import User exposing (User)
import ValidationMessage exposing (ValidationMessage)


type alias Model =
    { email : String
    , password : String
    , confirmPassword : String
    , validationMessage : ValidationMessage
    , session : Session
    , user : Maybe User
    }


type Msg
    = InputReceived InputType String
    | SignUpButtonClicked
    | SignUpMsgReceived Ports.InMessage


type InputType
    = Email
    | Password
    | ConfirmPassword



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    ( { email = ""
      , password = ""
      , confirmPassword = ""
      , validationMessage = ValidationMessage.create ""
      , session = session
      , user = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputReceived inputType value ->
            case inputType of
                Email ->
                    ( { model | email = value }, Cmd.none )

                Password ->
                    ( { model | password = value }, Cmd.none )

                ConfirmPassword ->
                    ( { model | confirmPassword = value }, Cmd.none )

        SignUpButtonClicked ->
            if model.password == model.confirmPassword then
                Credentials.create (Email.create model.email) (Password.create model.password)
                    |> Ports.signUp
                    |> (\cmd -> ( model, cmd ))

            else
                ValidationMessage.create "Password and confirm password must match."
                    |> (\message -> { model | validationMessage = message })
                    |> (\msgModel -> ( msgModel, Cmd.none ))

        SignUpMsgReceived message ->
            case Ports.stringToInOperation message.operation of
                Ports.AuthMsgReceived ->
                    addValidationMessage message.content model
                        |> (\updated -> ( updated, Cmd.none ))

                Ports.AuthStateChanged ->
                    resolveAuthChange message.content model

                _ ->
                    ( model, Cmd.none )


addValidationMessage : Maybe Decode.Value -> Model -> Model
addValidationMessage message model =
    case message of
        Just msg ->
            ValidationMessage.decode msg
                |> (\decoded -> { model | validationMessage = decoded })

        Nothing ->
            model


resolveAuthChange : Maybe Decode.Value -> Model -> ( Model, Cmd Msg )
resolveAuthChange maybeUser model =
    case maybeUser of
        Just jsonUser ->
            case User.decode jsonUser of
                Ok user ->
                    Route.toAbsolute (Just Route.Loading)
                        |> Nav.pushUrl (Session.getKey model.session)
                        |> (\cmd -> ( { model | user = Just user }, cmd ))

                Err error ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> DisplayData Msg
view model =
    DisplayData.create "Sign up!" (body model) Nothing


body : Model -> Element Msg
body model =
    column
        [ width shrink
        , width shrink
        , spacing 10
        , centerX
        , centerY
        , CustomHtmlEvents.onEnter SignUpButtonClicked
        ]
        [ validationMessage model.validationMessage
        , emailInput model.email
        , passwordInput model.password
        , confirmPasswordInput model.confirmPassword
        , signUpButton
        , signInLink
        ]


validationMessage : ValidationMessage -> Element msg
validationMessage message =
    ValidationMessage.getMessage message
        |> text
        |> el
            [ width shrink
            , centerX
            , Font.color Appearance.siteLightFontColor
            ]


emailInput : String -> Element Msg
emailInput email =
    Input.username
        [ Input.focusedOnLoad
        , htmlAttribute <| Html.Attributes.placeholder "Email Address"
        ]
        { onChange = InputReceived Email
        , text = email
        , placeholder = Nothing
        , label = Input.labelHidden "Email address"
        }


passwordInput : String -> Element Msg
passwordInput password =
    Input.newPassword
        [ htmlAttribute <| Html.Attributes.placeholder "Password"
        ]
        { onChange = InputReceived Password
        , text = password
        , placeholder = Nothing
        , label = Input.labelHidden "Password"
        , show = False
        }


confirmPasswordInput : String -> Element Msg
confirmPasswordInput password =
    Input.newPassword
        [ htmlAttribute <| Html.Attributes.placeholder "Confirm password"
        ]
        { onChange = InputReceived ConfirmPassword
        , text = password
        , placeholder = Nothing
        , label = Input.labelHidden "Confirm Password"
        , show = False
        }


signUpButton : Element Msg
signUpButton =
    Input.button
        [ centerX
        , height shrink
        , padding 5
        , Border.width 2
        , Border.rounded 5
        , Background.color Appearance.siteBackgroundDark
        , Font.color Appearance.siteLightFontColor
        ]
        { onPress = Just SignUpButtonClicked
        , label = text "Sign up!"
        }


signInLink : Element msg
signInLink =
    row
        [ Font.color Appearance.siteLightFontColor
        , centerX
        , width shrink
        ]
        [ el [] <|
            text "Oops! I already have an account. "
        , link
            [ Font.bold
            , Font.underline
            ]
            { url = Link.getUrl SignIn
            , label = text "Sign in!"
            }
        ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Ports.incomingMessage SignUpMsgReceived



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toUser : Model -> Maybe User
toUser model =
    model.user
