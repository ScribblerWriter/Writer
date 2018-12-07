port module Main exposing (Model, main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Json.Decode as JsonD
import Json.Encode as JsonE


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { dunno : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dunno = 0 }, Cmd.none )


type Msg
    = AddTargetButtonClicked



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTargetButtonClicked ->
            ( model
            , outgoingMessage <| { operation = "SaveTarget", collection = "targets", content = encodeSaveObject }
            )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Message =
    { operation : String
    , collection : String
    , content : JsonE.Value
    }


port outgoingMessage : Message -> Cmd msg


port incomingMessage : (Message -> msg) -> Sub msg


encodeSaveObject : JsonE.Value
encodeSaveObject =
    JsonE.object
        [ ( "name", JsonE.string "Angry Viking" )
        , ( "minutes", JsonE.int 25 )
        ]



-- view


view : Model -> Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Font.size 14
        ]
    <|
        Input.button
            []
            { onPress = Just AddTargetButtonClicked
            , label = text <| "Click me!"
            }
