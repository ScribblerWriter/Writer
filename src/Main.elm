port module Main exposing (Model, init, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JsonD
import Json.Encode as JsonE
import List.Extra
import Task
import Time



-- Main


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- counting data


type CountMethod
    = Additive
    | Subtractive



-- monster data


type alias Target =
    { name : String
    , imgSource : String
    , portraitSource : String
    , winCount : Int
    , minutes : Int
    }


availableTargets : Dict String Target
availableTargets =
    Dict.fromList
        [ ( "Front-Ax Viking"
          , { name = "Front-Ax Viking"
            , imgSource = "./images/viking1.png"
            , portraitSource = "./images/viking1portrait.png"
            , winCount = 25
            , minutes = 1
            }
          )
        , ( "Tough-Guy Viking"
          , { name = "Tough-Guy Viking"
            , imgSource = "./images/viking2.png"
            , portraitSource = "./images/viking2portrait.png"
            , winCount = 300
            , minutes = 30
            }
          )
        , ( "Viking Queen"
          , { name = "Viking Queen"
            , imgSource = "./images/viking3.png"
            , portraitSource = "./images/viking3portrait.png"
            , winCount = 1000
            , minutes = 100
            }
          )
        , ( "Stoic Viking"
          , { name = "Stoic Viking"
            , imgSource = "./images/viking4.png"
            , portraitSource = "./images/viking4portrait.png"
            , winCount = 200
            , minutes = 20
            }
          )
        , ( "Happy Viking"
          , { name = "Happy Viking"
            , imgSource = "./images/viking5.png"
            , portraitSource = "./images/viking5portrait.png"
            , winCount = 600
            , minutes = 60
            }
          )
        , ( "Dancing Viking"
          , { name = "Dancing Viking"
            , imgSource = "./images/viking6.png"
            , portraitSource = "./images/viking6portrait.png"
            , winCount = 150
            , minutes = 15
            }
          )
        , ( "Staunch Viking"
          , { name = "Staunch Viking"
            , imgSource = "./images/viking7.png"
            , portraitSource = "./images/viking7portrait.png"
            , winCount = 700
            , minutes = 70
            }
          )
        , ( "Angry Viking"
          , { name = "Angry Viking"
            , imgSource = "./images/viking8.png"
            , portraitSource = "./images/viking8portrait.png"
            , winCount = 900
            , minutes = 90
            }
          )
        , ( "Side-Ax Viking"
          , { name = "Side-Ax Viking"
            , imgSource = "./images/viking9.png"
            , portraitSource = "./images/viking9portrait.png"
            , winCount = 350
            , minutes = 35
            }
          )
        ]



-- Model


type alias Model =
    { writtenCount : Int
    , winProgress : Int
    , actualWordsAtLastCheck : Int
    , currentText : String
    , countMethod : CountMethod
    , currentTarget : Maybe Target
    , currentTargetTimerInSecs : Int
    , touched : Bool
    , showTargetSelector : Bool
    , windowDimensions : Dimensions
    , endMessage : String
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


init : JsonD.Value -> ( Model, Cmd Msg )
init flags =
    ( { writtenCount = 0
      , winProgress = 0
      , actualWordsAtLastCheck = 0
      , currentText = ""
      , countMethod = Additive
      , currentTarget = Nothing
      , currentTargetTimerInSecs = 0
      , touched = False
      , showTargetSelector = False
      , windowDimensions = getDimensions flags
      , endMessage = ""
      }
    , loadText ()
    )



-- Flag decoding


loggingDecoder : JsonD.Decoder a -> JsonD.Decoder a
loggingDecoder realDecoder =
    JsonD.value
        |> JsonD.andThen
            (\value ->
                case JsonD.decodeValue realDecoder value of
                    Ok decoded ->
                        JsonD.succeed decoded

                    Err error ->
                        JsonD.fail <| Debug.log "decode error" <| JsonD.errorToString error
            )


getDimensions : JsonD.Value -> Dimensions
getDimensions value =
    case JsonD.decodeValue (loggingDecoder dimensionDecoder) value of
        Ok dimensions ->
            dimensions

        Err _ ->
            { width = 0, height = 0 }


dimensionDecoder : JsonD.Decoder Dimensions
dimensionDecoder =
    JsonD.map2 Dimensions
        (JsonD.field "width" JsonD.int)
        (JsonD.field "height" JsonD.int)



-- Update


type Msg
    = WordsWritten String
    | CountMethodSelected CountMethod
    | StartButtonClicked String
    | SaveTimerTicked Time.Posix
    | LocalStorageLoaded (Maybe String)
    | TargetClicked
    | CancelTargetPickButtonClicked
    | WindowResized Int Int
    | TargetTimerTicked Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordsWritten document ->
            ( updateCounts document model, Cmd.none )

        CountMethodSelected method ->
            ( { model | countMethod = method }, Cmd.none )

        StartButtonClicked name ->
            let
                currentTarget : Maybe Target
                currentTarget =
                    availableTargets |> Dict.get name
            in
            ( { model
                | currentTarget = currentTarget
                , winProgress = 0
                , showTargetSelector = False
                , currentTargetTimerInSecs =
                    case currentTarget of
                        Nothing ->
                            0

                        Just target ->
                            target.minutes * 60
              }
            , Cmd.none
            )

        SaveTimerTicked _ ->
            ( { model | touched = False }
            , saveText <| encodeSaveObject model
            )

        LocalStorageLoaded content ->
            case content of
                Just data ->
                    ( { model
                        | currentText = getValue textDecoder data "Error loading text"
                        , writtenCount = getValue wordCountDecoder data -1
                        , countMethod = getValue methodDecoder data Additive
                        , actualWordsAtLastCheck = getValue actualCountDecoder data 0
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        TargetClicked ->
            ( { model | showTargetSelector = True }
            , Cmd.none
            )

        CancelTargetPickButtonClicked ->
            ( { model | showTargetSelector = False }
            , Cmd.none
            )

        WindowResized width height ->
            ( { model | windowDimensions = { width = width, height = height } }
            , Cmd.none
            )

        TargetTimerTicked _ ->
            case model.currentTarget of
                Nothing ->
                    ( model, Cmd.none )

                Just target ->
                    if model.currentTargetTimerInSecs <= 0 then
                        ( { model | endMessage = endFight model TimeExpired }, Cmd.none )

                    else
                        ( { model | currentTargetTimerInSecs = model.currentTargetTimerInSecs - 1 }, Cmd.none )


updateCounts : String -> Model -> Model
updateCounts document model =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document

        dif : Int
        dif =
            trimmedWordCount - model.actualWordsAtLastCheck
    in
    { model
        | writtenCount =
            if dif > 0 then
                model.writtenCount + dif

            else if model.countMethod == Additive then
                model.writtenCount

            else
                trimmedWordCount
        , actualWordsAtLastCheck = trimmedWordCount
        , winProgress =
            case model.currentTarget of
                Just target ->
                    if dif > 0 then
                        if model.winProgress + dif >= target.winCount then
                            target.winCount

                        else
                            model.winProgress + dif

                    else
                        model.winProgress

                Nothing ->
                    0
        , endMessage =
            case model.currentTarget of
                Just target ->
                    if dif > 0 then
                        if model.winProgress + dif >= target.winCount then
                            endFight model WordsReached

                        else
                            model.endMessage

                    else
                        model.endMessage

                Nothing ->
                    ""
        , currentText = document
        , touched = True
    }


countWords : String -> Int
countWords document =
    document
        |> String.replace "—" " "
        |> String.replace "–" " "
        |> String.words
        |> List.filter (String.any Char.isAlphaNum)
        |> List.length


type EndReason
    = TimeExpired
    | WordsReached


endFight : Model -> EndReason -> String
endFight model reason =
    case reason of
        TimeExpired ->
            "Time's up!"

        WordsReached ->
            "You win!"



-- JSON encoder/decoder


encodeSaveObject : Model -> String
encodeSaveObject model =
    JsonE.encode
        0
        (JsonE.object
            [ ( "count", JsonE.int model.writtenCount )
            , ( "text", JsonE.string model.currentText )
            , ( "method", methodEncoder model.countMethod )
            , ( "actualCount", JsonE.int model.actualWordsAtLastCheck )
            ]
        )


methodEncoder : CountMethod -> JsonE.Value
methodEncoder method =
    case method of
        Additive ->
            JsonE.string "additive"

        Subtractive ->
            JsonE.string "subtractive"


getValue : JsonD.Decoder a -> String -> a -> a
getValue decoder string errorVal =
    case JsonD.decodeString decoder string of
        Err _ ->
            errorVal

        Ok value ->
            value



-- Monster decoding


wordCountDecoder : JsonD.Decoder Int
wordCountDecoder =
    JsonD.field "count" JsonD.int


textDecoder : JsonD.Decoder String
textDecoder =
    JsonD.field "text" JsonD.string


actualCountDecoder : JsonD.Decoder Int
actualCountDecoder =
    JsonD.field "actualCount" JsonD.int


methodDecoder : JsonD.Decoder CountMethod
methodDecoder =
    JsonD.field "method" JsonD.string
        |> JsonD.andThen
            (\str ->
                case str of
                    "additive" ->
                        JsonD.succeed Additive

                    "subtractive" ->
                        JsonD.succeed Subtractive

                    wrongValue ->
                        JsonD.fail ("Count method decoding failed. Value: " ++ wrongValue)
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 SaveTimerTicked
        , textLoaded LocalStorageLoaded
        , Browser.Events.onResize WindowResized
        , Time.every 1000 TargetTimerTicked
        ]


port saveText : String -> Cmd msg


port loadText : () -> Cmd msg


port textLoaded : (Maybe String -> msg) -> Sub msg



-- View


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
        column
            [ width fill
            , height fill
            ]
            [ showTopMenu model
            , row
                [ width fill
                , height fill
                ]
                [ el
                    [ width <| px 5
                    , height fill
                    , Background.color <| rgb255 13 70 113
                    ]
                    none
                , if model.showTargetSelector then
                    showTargetSelector model

                  else
                    showEditor model
                , el
                    [ width <| px 5
                    , height fill
                    , Background.color <| rgb255 13 70 113
                    ]
                    none
                ]
            , el
                [ width fill
                , height <| px 5
                , Background.color <| rgb255 13 70 113
                ]
                none
            ]


showEditor : Model -> Element Msg
showEditor model =
    column
        [ width fill
        , height fill
        ]
        [ case model.currentTarget of
            Nothing ->
                none

            Just target ->
                showProgressBar model target
        , Input.multiline
            [ width fill
            , height fill
            , padding 25
            , Border.width 0
            , Border.rounded 0
            , htmlAttribute <| Html.Attributes.placeholder "Write your words here!"
            ]
            { onChange = WordsWritten
            , text = model.currentText
            , placeholder = Nothing
            , label = Input.labelHidden ""
            , spellcheck = False
            }
        ]


showTargetSelector : Model -> Element Msg
showTargetSelector model =
    let
        imageWidth : Int
        imageWidth =
            150

        imagesPerRow : Int
        imagesPerRow =
            model.windowDimensions.width // (imageWidth + 10)
    in
    column
        [ Background.color <| rgb255 108 160 229
        , width fill
        , height fill
        , padding 25
        ]
    <|
        buildTargetRows
            imagesPerRow
            imageWidth
            (Dict.values availableTargets)


buildTargetRows : Int -> Int -> List Target -> List (Element Msg)
buildTargetRows imagesPerRow imageWidth targets =
    let
        targetRows : List (List Target)
        targetRows =
            List.Extra.greedyGroupsOf imagesPerRow targets
    in
    List.map (buildSingleTargetRow imageWidth) targetRows


buildSingleTargetRow : Int -> List Target -> Element Msg
buildSingleTargetRow imageWidth targets =
    row
        [ padding 5 ]
    <|
        List.map
            (buildSingleTargetSelector imageWidth)
            targets


buildSingleTargetSelector : Int -> Target -> Element Msg
buildSingleTargetSelector imageWidth target =
    el
        [ Events.onClick (StartButtonClicked target.name)
        , pointer
        ]
    <|
        column
            []
            [ image
                [ width <| px imageWidth ]
                { src = target.imgSource
                , description = target.name
                }
            , el [ centerX ] <| text target.name
            , el [ centerX ] <| text <| String.fromInt target.winCount ++ " words"
            ]


showActionButton : String -> Msg -> Element Msg
showActionButton caption msg =
    Input.button
        [ centerX
        , centerY
        , padding 2
        , Border.width 2
        , Border.rounded 2
        , Background.color <| rgb255 78 222 37
        , Font.color <| rgb255 240 240 240
        ]
        { onPress = Just msg
        , label = text caption
        }


showTopMenu : Model -> Element Msg
showTopMenu model =
    row
        [ width fill
        , height <| px 50
        , Background.color <| rgb255 13 70 113
        , inFront <|
            if model.showTargetSelector then
                showActionButton "CANCEL" CancelTargetPickButtonClicked

            else
                showActionButton "TARGET!" TargetClicked
        ]
        [ el
            [ padding 10
            , centerY
            , Font.color <| rgb255 240 240 240
            ]
          <|
            text <|
                "Written so far: "
                    ++ String.fromInt model.writtenCount
        ]


showProgressBar : Model -> Target -> Element Msg
showProgressBar model target =
    row
        [ width fill
        , height <| px 38
        , centerY
        , inFront <|
            image
                [ width <| px 35
                , centerX
                , onLeft <|
                    el
                        [ alignRight, centerY ]
                    <|
                        text <|
                            target.name
                                ++ "  "
                , onRight <|
                    row [ centerY ]
                        [ el
                            [ alignLeft
                            , width shrink
                            ]
                          <|
                            text <|
                                "  "
                                    ++ String.fromInt model.winProgress
                                    ++ " / "
                                    ++ String.fromInt target.winCount
                        , el
                            [ alignLeft
                            ]
                          <|
                            text <|
                                "  "
                                    ++ (if model.endMessage /= "" then
                                            model.endMessage

                                        else
                                            formatSecondsToString model.currentTargetTimerInSecs
                                       )
                        ]
                ]
                { src = target.portraitSource
                , description = target.name ++ " portrait"
                }
        ]
        [ el
            [ width <| fillPortion model.winProgress
            , height <| px 20
            , Background.color <| rgb255 78 222 37
            ]
            none
        , el
            [ width <| fillPortion <| target.winCount - model.winProgress
            , height <| px 20
            , Background.color <| rgb255 200 200 200
            ]
            none
        ]


formatSecondsToString : Int -> String
formatSecondsToString seconds =
    if seconds < 60 then
        String.padLeft 2 '0' (String.fromInt seconds)

    else if seconds < 3600 then
        String.padLeft 2 '0' (String.fromInt (seconds // 60))
            ++ ":"
            ++ formatSecondsToString (remainderBy 60 seconds)

    else
        String.fromInt (seconds // 3600)
            ++ ":"
            ++ formatSecondsToString (remainderBy 3600 seconds)


viewOld model =
    Element.layout
        [ Font.size 14
        , padding 5
        , inFront
            (if model.showTargetSelector then
                showMonsterPickerOld

             else
                none
            )
        ]
    <|
        column
            [ width fill
            , height fill
            ]
            [ el
                [ Region.heading 1
                , centerX
                , Font.size 24
                ]
                (text "The Amazing Text Combatotron")
            , row
                [ width fill
                , height fill
                ]
                [ column
                    [ width fill
                    , height fill
                    ]
                    [ row
                        [ width fill
                        , padding 5
                        ]
                        [ el
                            [ width fill ]
                            (text ("Current count: " ++ String.fromInt model.writtenCount))
                        , el
                            [ width fill
                            , centerX
                            ]
                            (text
                                (if model.touched then
                                    "Saving..."

                                 else
                                    "Saved."
                                )
                            )
                        , Input.radioRow
                            [ spacing 20 ]
                            { onChange = CountMethodSelected
                            , selected = Just model.countMethod
                            , label = Input.labelLeft [] (text "")
                            , options =
                                [ Input.option Additive (el [ alignRight ] (text "Additive"))
                                , Input.option Subtractive (el [ alignRight ] (text "Subtractive"))
                                ]
                            }
                        ]
                    , Input.multiline
                        [ width fill
                        , height fill
                        ]
                        { onChange = WordsWritten
                        , text = model.currentText
                        , placeholder = Nothing
                        , label = Input.labelLeft [] (text "")
                        , spellcheck = False
                        }
                    ]
                , column
                    [ width (px 300)
                    , alignTop
                    ]
                    [ Input.button
                        [ centerX
                        , padding 4
                        , Border.rounded 2
                        , Border.width 1
                        , Border.solid
                        , Border.shadow
                            { offset = ( 1, 1 )
                            , size = 1
                            , blur = 5
                            , color = rgb255 0 0 0
                            }
                        ]
                        { onPress = Just TargetClicked
                        , label = text "Fight Something!"
                        }
                    , showMonster model
                    ]
                ]
            ]


showMonster : Model -> Element Msg
showMonster model =
    case model.currentTarget of
        Nothing ->
            Element.none

        Just monster ->
            column
                [ width fill
                , alignTop
                ]
                [ image
                    [ width fill
                    , inFront
                        (if model.winProgress >= monster.winCount then
                            image
                                [ width fill
                                ]
                                { src = "./images/explosion.gif"
                                , description = "You win!"
                                }

                         else
                            none
                        )
                    ]
                    { src = monster.imgSource
                    , description = monster.name
                    }
                , row
                    [ width fill
                    , inFront
                        (el
                            [ centerX
                            , centerY
                            , width shrink
                            , height shrink
                            , Font.size 20
                            ]
                            (text (String.fromInt model.winProgress ++ " / " ++ String.fromInt monster.winCount))
                        )
                    ]
                    [ el
                        [ width (fillPortion model.winProgress)
                        , Background.color (rgb255 10 240 10)
                        , height (px 30)
                        ]
                        none
                    , el
                        [ width (fillPortion (monster.winCount - model.winProgress))
                        , height (px 30)
                        , Background.color (rgb255 200 200 200)
                        ]
                        none
                    ]
                ]


showMonsterPickerOld : Element Msg
showMonsterPickerOld =
    el
        [ padding 5
        , Background.color (rgb255 255 221 130)
        , centerX
        , centerY
        , width shrink
        , height <| px 200
        ]
    <|
        column
            []
            [ row
                [ width fill ]
                [ el [ width fill ] none
                , el
                    [ width shrink
                    , alignRight
                    , padding 5
                    , Border.rounded 10
                    , Border.width 2
                    , Border.solid
                    , Font.size 20
                    , Events.onClick CancelTargetPickButtonClicked
                    , pointer
                    ]
                  <|
                    text "X"
                ]
            , row
                [ padding 5
                , centerX
                , centerY
                , width fill
                ]
              <|
                List.map showMonsterItem (Dict.values availableTargets)
            ]


showMonsterItem : Target -> Element Msg
showMonsterItem target =
    column
        [ width <| px 130
        , centerX
        ]
        [ image
            [ width <| px 100
            , centerX
            , Events.onClick (StartButtonClicked target.name)
            , pointer
            ]
            { src = target.imgSource
            , description = target.name
            }
        , el [ centerX ] <| text target.name
        , el [ centerX ] <| text <| String.fromInt target.winCount ++ " words"
        ]
