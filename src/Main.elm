module Main exposing (Model, init, main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)



-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type CountMethod
    = Additive
    | Subtractive


type alias Monster =
    { name : String
    , imgSource : String
    , killCount : Int
    }


type alias Model =
    { writtenCount : Int
    , killProgress : Int
    , actualWordsAtLastCheck : Int
    , countMethod : CountMethod
    , currentMonster : Maybe Monster
    }


availableMonsters : List Monster
availableMonsters =
    [ { name = "Bezos the Destroyer"
      , imgSource = "images/bezos.png"
      , killCount = 25
      }
    ]


init : Model
init =
    { writtenCount = 0
    , killProgress = 0
    , actualWordsAtLastCheck = 0
    , countMethod = Additive
    , currentMonster = Nothing
    }



-- Update


type Msg
    = UpdateCount String
    | SetCountMethod CountMethod
    | StartFight


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCount document ->
            updateCounts document model

        SetCountMethod method ->
            { model | countMethod = method }

        StartFight ->
            { model
                | currentMonster = List.head availableMonsters
                , killProgress = 0
            }


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
    }


countWords : String -> Int
countWords document =
    document
        |> String.replace "—" " "
        |> String.replace "–" " "
        |> String.words
        |> List.filter (String.any Char.isAlphaNum)
        |> List.length



-- View


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 14
        , padding 5
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
                        , text = ""
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
                        { onPress = Just StartFight
                        , label = text "Start fight!"
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
                                { src = "images/explosion.gif"
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
                            , width (px 300)
                            , height (px 25)
                            , Font.size 20
                            ]
                            (text (String.fromInt model.killProgress ++ " / " ++ String.fromInt monster.killCount))
                        )
                    ]
                    [ el
                        [ width (fillPortion model.killProgress)
                        , Background.color (rgb255 10 240 10)
                        , height (px 25)
                        ]
                        none
                    , el
                        [ width (fillPortion (monster.killCount - model.killProgress))
                        , height (px 25)
                        , Background.color (rgb255 200 200 200)
                        ]
                        none
                    ]
                ]
