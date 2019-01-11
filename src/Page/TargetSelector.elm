module Page.TargetSelector exposing (Model, Msg, init, update, view)

import Appearance
import Data.Target exposing (Target)
import Dict exposing (Dict)
import Element exposing (..)
import Ports
import Skeleton
import State exposing (State)


type alias Model =
    { targets : Dict String Target }


init : ( Model, Cmd Msg )
init =
    ( { targets = Dict.empty }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        NoOp ->
            ( state, ( model, Cmd.none ) )



-- View


view : State -> Model -> Skeleton.PageData Msg
view state model =
    { title = "Target Selector"
    , headerSettings = Just (getHeaderSettings state)
    , body = showBody model
    }


getHeaderSettings : State -> Skeleton.HeaderSettings
getHeaderSettings state =
    { writtenCount = state.writtenCount
    , actionButtonSettings =
        Just { action = "/", label = "CANCEL" }
    , signOutButtonSettings =
        Just { action = "/signout", label = "Sign Out" }
    }


showBody : Model -> Element Msg
showBody model =
    none
