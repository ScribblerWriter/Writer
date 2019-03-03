module Page.Authenticator exposing (Model, Msg, init, subscriptions, update, view)

import Appearance
import Browser
import Browser.Events
import CustomHtmlEvents
import DisplayMessage exposing (Message)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Json.Encode as Encode
import Ports
import Skeleton
import State exposing (State)


type alias Model =
    { email : String
    , password : String
    , currentSignUpPage : SignUpPage
    }


type InputType
    = Email
    | Password


type SignUpPage
    = None
    | UserPass


type Msg
    = SignInInputReceived InputType String
    | SignInButtonClicked
    | SignUpButtonClicked
    | CreateUserButtonClicked
    | ReturnToSignInButtonClicked


type alias FieldValues =
    { buttonText : String
    , clickEvent : Msg
    , underText : String
    , underClickEvent : Msg
    , underButtonText : String
    }



-- init


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , currentSignUpPage = None
      }
    , Cmd.none
    )



-- update


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        SignInInputReceived inputType value ->
            case inputType of
                Email ->
                    ( state, ( { model | email = value }, Cmd.none ) )

                Password ->
                    ( state, ( { model | password = value }, Cmd.none ) )

        SignInButtonClicked ->
            clearAuthMessages state.messages
                |> (\messages ->
                        ( { state | messages = messages }
                        , ( model
                          , Ports.sendMessageWithJustContent
                                Ports.SignIn
                                (emailPassEncoder model.email model.password)
                          )
                        )
                   )

        SignUpButtonClicked ->
            clearAuthMessages state.messages
                |> (\messages ->
                        ( { state | messages = messages }
                        , ( { email = ""
                            , password = ""
                            , currentSignUpPage = UserPass
                            }
                          , Cmd.none
                          )
                        )
                   )

        ReturnToSignInButtonClicked ->
            clearAuthMessages state.messages
                |> (\messages ->
                        ( { state | messages = messages }
                        , ( { email = ""
                            , password = ""
                            , currentSignUpPage = None
                            }
                          , Cmd.none
                          )
                        )
                   )

        CreateUserButtonClicked ->
            clearAuthMessages state.messages
                |> (\messages ->
                        ( { state | messages = messages }
                        , ( model
                          , Ports.sendMessageWithJustContent
                                Ports.SignUp
                                (emailPassEncoder model.email model.password)
                          )
                        )
                   )


clearAuthMessages : List Message -> List Message
clearAuthMessages messages =
    messages
        |> List.filter (\msg -> msg.source /= DisplayMessage.Auth)



-- View


view : Model -> List Message -> Skeleton.PageData Msg
view model messages =
    { title = "Sign In"
    , headerSettings = Nothing
    , body = showBody model messages
    }


showBody : Model -> List Message -> Element Msg
showBody model messages =
    case model.currentSignUpPage of
        None ->
            showDetails model
                messages
                { buttonText = "Sign in!"
                , clickEvent = SignInButtonClicked
                , underText = "Don't have an account yet? "
                , underClickEvent = SignUpButtonClicked
                , underButtonText = "Sign up!"
                }

        UserPass ->
            showDetails model
                messages
                { buttonText = "Sign up!"
                , clickEvent = CreateUserButtonClicked
                , underText = "Oops, I already have an account! "
                , underClickEvent = ReturnToSignInButtonClicked
                , underButtonText = "Go back!"
                }


showDetails : Model -> List Message -> FieldValues -> Element Msg
showDetails model messages fieldValues =
    column
        [ width shrink
        , width shrink
        , spacing 10
        , centerX
        , centerY
        , CustomHtmlEvents.onEnter fieldValues.clickEvent
        ]
        [ validationMessages messages
        , Input.username
            [ Input.focusedOnLoad
            , htmlAttribute <| Html.Attributes.placeholder "Email Address"
            ]
            { onChange = SignInInputReceived Email
            , text = model.email
            , placeholder = Nothing
            , label = Input.labelHidden "Email address"
            }
        , Input.currentPassword
            [ htmlAttribute <| Html.Attributes.placeholder "Password"
            ]
            { onChange = SignInInputReceived Password
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelHidden "Password"
            , show = False
            }
        , Input.button
            [ centerX
            , height shrink
            , padding 5
            , Border.width 2
            , Border.rounded 5
            , Background.color Appearance.siteBackgroundDark
            , Font.color Appearance.siteLightFontColor
            ]
            { onPress = Just fieldValues.clickEvent
            , label = text fieldValues.buttonText
            }
        , row
            [ Font.color Appearance.siteLightFontColor
            , centerX
            , width shrink
            ]
            [ el [] <|
                text fieldValues.underText
            , Input.button
                [ Font.bold
                , Font.underline
                ]
                { onPress = Just fieldValues.underClickEvent
                , label = text fieldValues.underButtonText
                }
            ]
        ]


validationMessages : List Message -> Element msg
validationMessages messages =
    column
        [ width shrink
        , centerX
        , Font.color Appearance.siteLightFontColor
        ]
    <|
        parseMessages messages


parseMessages : List Message -> List (Element msg)
parseMessages messages =
    messages
        |> List.filter (\msg -> msg.source == DisplayMessage.Auth)
        |> List.map (\msg -> el [] <| text <| textForMessage msg)


textForMessage : Message -> String
textForMessage message =
    case message.code of
        DisplayMessage.BadPass ->
            badUserOrPass

        DisplayMessage.BadUser ->
            badUserOrPass

        DisplayMessage.InvalidEmail ->
            message.body

        DisplayMessage.WeakPassword ->
            message.body

        DisplayMessage.EmailAlreadyInUse ->
            message.body

        _ ->
            "Unhandled message: " ++ message.body


badUserOrPass : String
badUserOrPass =
    "Invalid email address or password."



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
