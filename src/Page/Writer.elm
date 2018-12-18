module Page.Writer exposing (Model, Msg, init, view)

import Element exposing (..)
import Skeleton


type alias Model =
    { writtenCount : Int }


type Msg
    = WordsWritten String


init : ( Model, Cmd Msg )
init =
    ( { writtenCount = 1 }, Cmd.none )


view : Model -> Skeleton.PageData Msg
view model =
    { title = "Writing page"
    , body = el [] <| text "This is the writing page."
    }
