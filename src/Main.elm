module Main exposing (Model, init, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type CountMethod
    = Additive
    | Subtractive


type alias Model =
    { writtenCount : Int
    , actualWordsAtLastCheck : Int
    , countMethod : CountMethod
    }


init : Model
init =
    { writtenCount = 0
    , actualWordsAtLastCheck = 0
    , countMethod = Additive
    }



-- Update


type Msg
    = UpdateCount String
    | SetCountMethod CountMethod


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCount document ->
            if model.countMethod == Additive then
                countWordsAdditive document model

            else
                countWordsSubtractive document model

        SetCountMethod method ->
            { model | countMethod = method }


countWordsAdditive : String -> Model -> Model
countWordsAdditive document model =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document

        dif : Int
        dif =
            trimmedWordCount - model.actualWordsAtLastCheck
    in
    if dif > 0 then
        { model
            | writtenCount = model.writtenCount + dif
            , actualWordsAtLastCheck = trimmedWordCount
        }

    else
        { model | actualWordsAtLastCheck = trimmedWordCount }


countWordsSubtractive : String -> Model -> Model
countWordsSubtractive document model =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document
    in
    { model | writtenCount = trimmedWordCount, actualWordsAtLastCheck = trimmedWordCount }


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
    div []
        [ div []
            [ radio (SetCountMethod Additive) "Additive" "CountMethod" (model.countMethod == Additive)
            , radio (SetCountMethod Subtractive) "Subtractive" "CountMethod" (model.countMethod == Subtractive)
            ]
        , div [] [ text ("Current count: " ++ String.fromInt model.writtenCount) ]
        , textarea [ cols 80, rows 50, placeholder "Start writing here!", onInput UpdateCount ] []
        ]


radio : Msg -> String -> String -> Bool -> Html Msg
radio msg optionname groupname chosen =
    label []
        [ input [ type_ "radio", name groupname, checked chosen, onClick msg ] []
        , text optionname
        ]
