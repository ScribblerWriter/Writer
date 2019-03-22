module Page.SignIn exposing (Model, Msg, init, subscriptions, update, view)

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
    , validationMessage : ValidationMessage
    }


type Msg
    = InputReceived InputType String
    | SignInButtonClicked
    | AuthMsgReceived Ports.InMessage


type InputType
    = Email
    | Password



-- init


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
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

        SignInButtonClicked ->
            Credentials.create (Email.create model.email) (Password.create model.password)
                |> Credentials.encode
                |> Ports.sendMessageWithJustContent Ports.SignIn
                |> (\cmd -> ( model, cmd ))

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
    DisplayData.create "Sign in!" (body model) Nothing


body : Model -> Element Msg
body model =
    column
        [ width shrink
        , width shrink
        , spacing 10
        , centerX
        , centerY
        , CustomHtmlEvents.onEnter SignInButtonClicked
        ]
        [ validationMessage model.validationMessage
        , emailInput model.email
        , passwordInput model.password
        , signInButton
        , signUpLink
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
    Input.currentPassword
        [ htmlAttribute <| Html.Attributes.placeholder "Password"
        ]
        { onChange = InputReceived Password
        , text = password
        , placeholder = Nothing
        , label = Input.labelHidden "Password"
        , show = False
        }


signInButton : Element Msg
signInButton =
    Input.button
        [ centerX
        , height shrink
        , padding 5
        , Border.width 2
        , Border.rounded 5
        , Background.color Appearance.siteBackgroundDark
        , Font.color Appearance.siteLightFontColor
        ]
        { onPress = Just SignInButtonClicked
        , label = text "Sign in!"
        }


signUpLink : Element msg
signUpLink =
    row
        [ Font.color Appearance.siteLightFontColor
        , centerX
        , width shrink
        ]
        [ el [] <|
            text "Don't have an account yet? "
        , link
            [ Font.bold
            , Font.underline
            ]
            { url = Link.getUrl SignUp
            , label = text "Sign up!"
            }
        ]



-- subscriptions


subscriptions : Sub Msg
subscriptions =
    Ports.incomingMessage AuthMsgReceived
