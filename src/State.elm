module State exposing (State)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional)
import Settings exposing (Settings)


type alias State =
    { settings : Settings }
