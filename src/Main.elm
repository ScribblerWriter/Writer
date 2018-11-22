port module Main exposing (Model, init, main)

import Browser
import Dict exposing (Dict)
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


type alias Monster =
    { name : String
    , imgSource : String
    , killCount : Int
    }


availableMonsters : Dict String Monster
availableMonsters =
    Dict.fromList
        [ ( "Bezos the Destroyer"
          , { name = "Bezos the Destroyer"
            , imgSource = "./images/bezos.png"
            , killCount = 25
            }
          )
        , ( "KDP Support"
          , { name = "KDP Support"
            , imgSource = "./images/kdpsupport.png"
            , killCount = 10
            }
          )
        , ( "Carlos F"
          , { name = "Carlos F"
            , imgSource = "./images/carlosf.png"
            , killCount = 50
            }
          )
        ]



-- Model


type alias Model =
    { writtenCount : Int
    , killProgress : Int
    , actualWordsAtLastCheck : Int
    , currentText : String
    , countMethod : CountMethod
    , currentMonster : Maybe Monster
    , touched : Bool
    , showMonsterPicker : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { writtenCount = 0
      , killProgress = 0
      , actualWordsAtLastCheck = 0
      , currentText = ""
      , countMethod = Additive
      , currentMonster = Nothing
      , touched = False
      , showMonsterPicker = False
      }
    , loadText ()
    )



-- Update


type Msg
    = UpdateCount String
    | SetCountMethod CountMethod
    | StartFight String
    | SaveToLocal Time.Posix
    | LoadLocalComplete (Maybe String)
    | PickMonster
    | CancelMonsterPick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCount document ->
            ( updateCounts document model, Cmd.none )

        SetCountMethod method ->
            ( { model | countMethod = method }, Cmd.none )

        StartFight name ->
            ( { model
                | currentMonster = availableMonsters |> Dict.get name
                , killProgress = 0
                , showMonsterPicker = False
              }
            , Cmd.none
            )

        SaveToLocal frequency ->
            ( { model | touched = False }
            , saveText <| encodeSaveObject model
            )

        LoadLocalComplete content ->
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

        PickMonster ->
            ( { model | showMonsterPicker = True }
            , Cmd.none
            )

        CancelMonsterPick ->
            ( { model | showMonsterPicker = False }
            , Cmd.none
            )


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
        , killProgress =
            case model.currentMonster of
                Just monster ->
                    if dif > 0 then
                        if model.killProgress + dif >= monster.killCount then
                            monster.killCount

                        else
                            model.killProgress + dif

                    else
                        model.killProgress

                Nothing ->
                    0
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


getValue : JsonD.Decoder a -> String -> a -> a
getValue decoder string errorVal =
    case JsonD.decodeString decoder string of
        Err _ ->
            errorVal

        Ok value ->
            value



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 SaveToLocal
        , textLoaded LoadLocalComplete
        ]


port saveText : String -> Cmd msg


port loadText : () -> Cmd msg


port textLoaded : (Maybe String -> msg) -> Sub msg



-- View


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 14
        , padding 5
        , inFront
            (if model.showMonsterPicker then
                showMonsterPicker

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
                            { onChange = SetCountMethod
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
                        { onChange = UpdateCount
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
                        { onPress = Just PickMonster
                        , label = text "Fight Something!"
                        }
                    , showMonster model
                    ]
                ]
            ]


showMonster : Model -> Element Msg
showMonster model =
    case model.currentMonster of
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
                        (if model.killProgress >= monster.killCount then
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
                            (text (String.fromInt model.killProgress ++ " / " ++ String.fromInt monster.killCount))
                        )
                    ]
                    [ el
                        [ width (fillPortion model.killProgress)
                        , Background.color (rgb255 10 240 10)
                        , height (px 30)
                        ]
                        none
                    , el
                        [ width (fillPortion (monster.killCount - model.killProgress))
                        , height (px 30)
                        , Background.color (rgb255 200 200 200)
                        ]
                        none
                    ]
                ]


showMonsterPicker : Element Msg
showMonsterPicker =
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
                    , Events.onClick CancelMonsterPick
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
                List.map showMonsterItem (Dict.values availableMonsters)
            ]


showMonsterItem : Monster -> Element Msg
showMonsterItem monster =
    column
        [ width <| px 130
        , centerX
        ]
        [ image
            [ width <| px 100
            , centerX
            , Events.onClick (StartFight monster.name)
            , pointer
            ]
            { src = monster.imgSource
            , description = monster.name
            }
        , el [ centerX ] <| text monster.name
        , el [ centerX ] <| text <| String.fromInt monster.killCount ++ " words"
        ]
