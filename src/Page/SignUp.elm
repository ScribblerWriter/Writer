module Page.SignUp exposing (Model, Msg, init, subscriptions, update, view)

import Appearance
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
import ValidationMessage exposing (ValidationMessage)


type alias Model =
    { email : String
    , password : String
    , confirmPassword : String
    , validationMessage : ValidationMessage
    }


type Msg
    = InputReceived InputType String
    | SignUpButtonClicked
    | AuthMsgReceived Ports.InMessage


type InputType
    = Email
    | Password
    | ConfirmPassword



-- init


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , confirmPassword = ""
      , validationMessage = ValidationMessage.create ""
      }
    , Cmd.none
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "SigIn.update" msg of
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
                    |> Credentials.encode
                    |> Ports.sendMessageWithJustContent Ports.SignUp
                    |> (\cmd -> ( model, cmd ))

            else
                ValidationMessage.create "Password and confirm password must match."
                    |> (\message -> { model | validationMessage = message })
                    |> (\msgModel -> ( msgModel, Cmd.none ))

        AuthMsgReceived message ->
            case Ports.stringToInOperation message.operation of
                Ports.AuthMsgReceived ->
                    addValidationMessage message.content model
                        |> (\updated -> ( updated, Cmd.none ))

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



-- view


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



-- subscriptions


subscriptions : Sub Msg
subscriptions =
    Ports.incomingMessage AuthMsgReceived
