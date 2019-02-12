module Page.Settings exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Appearance
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Json.Encode as Encode
import Ports
import Skeleton
import State exposing (State)


type Msg
    = DisplayNameInputRecived String
    | SaveButtonPressed


type alias Model =
    { displayName : Maybe String }


init : ( Model, Cmd Msg )
init =
    ( { displayName = Nothing }, Cmd.none )



-- update


update : Msg -> Model -> State -> ( State, ( Model, Cmd Msg ) )
update msg model state =
    case msg of
        DisplayNameInputRecived text ->
            ( state
            , ( { model
                    | displayName = Just text
                }
              , Cmd.none
              )
            )

        SaveButtonPressed ->
            case state.user of
                Just user ->
                    ( state
                    , ( model
                      , Ports.sendMessageWithContentAndResponse
                            Ports.SaveToDb
                            (encodeSave user.uid model)
                            Ports.SettingsSaved
                      )
                    )

                Nothing ->
                    ( state, ( model, Cmd.none ) )



-- view


view : Model -> State -> Skeleton.PageData Msg
view model state =
    { title = "Settings"
    , headerSettings = Just { actionButtonSettings = Nothing }
    , body = showBody model state
    }


showBody : Model -> State -> Element Msg
showBody model state =
    column
        [ width fill
        , height fill
        , Background.color Appearance.siteBackgroundTargetSelection
        , padding 10
        , spacing 10
        ]
        [ displayName model
        , saveSettings
        ]


displayName : Model -> Element Msg
displayName model =
    Input.text []
        { onChange = DisplayNameInputRecived
        , text = Maybe.withDefault "" model.displayName
        , placeholder = Just <| Input.placeholder [] <| text "Display Name"
        , label =
            Input.labelLeft [ width <| fillPortion 5 ] <|
                paragraph
                    []
                    [ text "Your Display Name is your identity on GameYourWords.com. This name will be how you are seen by anyone else in the GameYourWords community. Please choose a name that isn't obscene or offensive." ]
        }


saveSettings : Element Msg
saveSettings =
    Input.button []
        { onPress = Just SaveButtonPressed
        , label = text "Save settings"
        }



-- encoding


encodeSave : String -> Model -> Encode.Value
encodeSave userId model =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "doc", Encode.string userId )
        , ( "data", encodeSettings model )
        ]


encodeSettings : Model -> Encode.Value
encodeSettings model =
    let
        fields : List ( String, Maybe Encode.Value )
        fields =
            [ ( "displayName", encodeField .displayName Encode.string )
            ]

        encodeField : (Model -> Maybe a) -> (a -> Encode.Value) -> Maybe Encode.Value
        encodeField selector encoder =
            selector model |> Maybe.map encoder

        liftMaybe : ( a, Maybe b ) -> Maybe ( a, b )
        liftMaybe ( first, second ) =
            Maybe.map (\second_ -> ( first, second_ )) second
    in
    List.filterMap liftMaybe fields
        |> Encode.object



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
