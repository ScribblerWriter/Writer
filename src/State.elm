module State exposing
    ( State
    , create
    , getActualCount
    , getAdditiveCount
    , getCurrentText
    , getSettings
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional)
import LocalState exposing (LocalState)
import Settings exposing (Settings)


type State
    = State
        { settings : Settings
        , additiveCount : Int
        , actualCount : Int
        , currentText : String
        }


create : Settings -> LocalState -> State
create settings localState =
    State
        { settings = settings
        , additiveCount = LocalState.getAdditiveCount localState
        , actualCount = LocalState.getActualCount localState
        , currentText = LocalState.getCurrentText localState
        }



-- ACCESSORS


getSettings : State -> Settings
getSettings (State { settings }) =
    settings


getAdditiveCount : State -> Int
getAdditiveCount (State { additiveCount }) =
    additiveCount


getActualCount : State -> Int
getActualCount (State { actualCount }) =
    actualCount


getCurrentText : State -> String
getCurrentText (State { currentText }) =
    currentText
