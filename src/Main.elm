module Main exposing (Model, init, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { writtenCount : Int
    , actualWordsAtLastCheck : Int
    }


init : Model
init =
    { writtenCount = 0
    , actualWordsAtLastCheck = 0
    }



-- Update


type Msg
    = UpdateCount String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCount words ->
            countWords words model


countWords : String -> Model -> Model
countWords words model =
    let
        dif : Int
        dif =
            List.length (String.words words) - model.actualWordsAtLastCheck
    in
    if dif > 0 then
        { model
            | writtenCount = model.writtenCount + dif
            , actualWordsAtLastCheck = List.length (String.words words)
        }

    else
        { model | actualWordsAtLastCheck = List.length (String.words words) }

trimNonWords : List String -> List String
trimNonWords

-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Current count: " ++ String.fromInt model.writtenCount) ]
        , textarea [ cols 40, rows 10, placeholder "Start writing here!", onInput UpdateCount ] []
        ]



{-

-}
