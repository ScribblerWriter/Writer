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
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Encode as Encode
import Ports
import Skeleton
import State exposing (State)


type Msg
    = DisplayNameInputRecived String
    | CountMethodChanged Bool
    | SaveButtonPressed


type alias Model =
    { displayName : Maybe String
    , countMethod : Maybe State.CountMethod
    }


init : ( Model, Cmd Msg )
init =
    ( { displayName = Nothing
      , countMethod = Nothing
      }
    , Cmd.none
    )



-- update


update : Msg -> Model -> State -> ( State, ( Model, Cmd Msg ) )
update msg model state =
    case msg of
        DisplayNameInputRecived text ->
            ( state
            , ( { model | displayName = Just text }
              , Cmd.none
              )
            )

        CountMethodChanged checked ->
            countMethodFromBool checked
                |> (\method ->
                        ( state
                        , ( { model | countMethod = Just method }
                          , Cmd.none
                          )
                        )
                   )

        SaveButtonPressed ->
            case state.user of
                Just user ->
                    ( state
                    , ( model
                      , Ports.sendMessageWithContentAndResponse
                            Ports.SaveToDbCollection
                            (encodeSave user.uid model)
                            Ports.SettingsSaved
                      )
                    )

                Nothing ->
                    ( state, ( model, Cmd.none ) )


countMethodFromBool : Bool -> State.CountMethod
countMethodFromBool bool =
    if bool then
        State.Additive

    else
        State.Subtractive


countMethodToBool : State.CountMethod -> Bool
countMethodToBool method =
    case method of
        State.Additive ->
            True

        State.Subtractive ->
            False



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
        , Background.color Appearance.siteBackgroundMediumDark
        , padding 10
        , spacing 10
        , Font.color Appearance.siteLightFontColor
        ]
        [ showSetting <| displayName <| touchedOrState .displayName model .displayName state.settings
        , showSetting <| countMethod <| touchedOrState .countMethod model .countMethod state.settings
        , saveSettingsButton
        ]


touchedOrState : (Model -> Maybe a) -> Model -> (State.Settings -> a) -> State.Settings -> a
touchedOrState modelSelector model settingsSelector settings =
    case modelSelector model of
        Just setting ->
            setting

        Nothing ->
            settingsSelector settings


type alias SettingEntry =
    { header : String
    , description : String
    , input : Element Msg
    }


showSetting : SettingEntry -> Element Msg
showSetting setting =
    column []
        [ el
            [ Font.size Appearance.siteHeaderSize
            , paddingEach { defaultPadding | bottom = 10 }
            ]
          <|
            text setting.header
        , row []
            [ el
                [ width <| fillPortion 4
                , paddingEach { defaultPadding | left = 10, right = 10 }
                , alignTop
                ]
              <|
                paragraph [] [ text setting.description ]
            , setting.input
            ]
        ]


displayName : String -> SettingEntry
displayName name =
    { header = "Display Name"
    , description = "Your Display Name is your identity on GameYourWords.com. This name will be how you are seen by anyone else in the GameYourWords community. Please choose a name that isn't obscene or offensive."
    , input =
        Input.text
            [ width <| fillPortion 1
            , Font.color Appearance.black
            , alignTop
            ]
            { onChange = DisplayNameInputRecived
            , text = name
            , placeholder = Just <| Input.placeholder [] <| text "Display Name"
            , label = Input.labelHidden "Display Name"
            }
    }


countMethod : State.CountMethod -> SettingEntry
countMethod method =
    { header = "Counting Method"
    , description = "Your words can be counted in two different ways, depending on your preference. 'New words' means that every word you type will be added to your word count. 'Actual words' means that if you delete words, they will also be removed from your word count. This only affects your daily word count. Progress towards completing a target will always count every word written."
    , input =
        Input.checkbox
            [ width <| fillPortion 1
            , centerX
            , alignTop
            ]
            { onChange = CountMethodChanged
            , icon = showCheckBox "New words" "Actual words"
            , checked = countMethodToBool method
            , label = Input.labelHidden "Counting Method"
            }
    }


showCheckBox : String -> String -> Bool -> Element msg
showCheckBox leftValue rightValue checked =
    let
        ( leftColor, rightColor ) =
            if checked then
                ( Appearance.siteActionButtonColor, Appearance.mediumGray )

            else
                ( Appearance.mediumGray, Appearance.siteActionButtonColor )
    in
    row
        [ Border.color Appearance.siteLightFontColor
        , width shrink
        , centerX
        , centerY
        , pointer
        ]
        [ el
            [ centerX
            , centerY
            , Background.color leftColor
            , Border.roundEach { topLeft = 5, bottomLeft = 5, topRight = 0, bottomRight = 0 }
            , Border.width 2
            , padding 10
            ]
          <|
            text leftValue
        , el
            [ centerX
            , centerY
            , Background.color rightColor
            , Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 5, bottomRight = 5 }
            , Border.widthEach { left = 0, top = 2, bottom = 2, right = 2 }
            , padding 10
            ]
          <|
            text rightValue
        ]


defaultPadding : { top : Int, bottom : Int, left : Int, right : Int }
defaultPadding =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


saveSettingsButton : Element Msg
saveSettingsButton =
    Input.button
        [ Border.color Appearance.siteLightFontColor
        , Border.width 2
        , Border.rounded 5
        , Background.color Appearance.siteActionButtonColor
        , padding 10
        ]
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
            , ( "countMethod", encodeField .countMethod State.methodEncoder )
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
