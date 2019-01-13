module Page.TargetSelector exposing (Model, Msg, init, update, view)

import Appearance
import Data.Target exposing (Target)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import List.Extra
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
    = TargetButtonClicked String


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        TargetButtonClicked name ->
            ( state, ( model, Cmd.none ) )



-- View


view : Model -> State -> Skeleton.PageData Msg
view model state =
    { title = "Target Selector"
    , headerSettings = Just (getHeaderSettings state)
    , body = showBody model state
    }


getHeaderSettings : State -> Skeleton.HeaderSettings
getHeaderSettings state =
    { writtenCount = state.writtenCount
    , actionButtonSettings =
        Just { action = "/", label = "CANCEL" }
    , signOutButtonSettings =
        Just { action = "/signout", label = "Sign Out" }
    }


showBody : Model -> State -> Element Msg
showBody model state =
    let
        imageWidth : Int
        imageWidth =
            150

        imagesPerRow : Int
        imagesPerRow =
            state.windowDimensions.width // (imageWidth + 10)
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
            (Dict.values model.targets)


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
        [ Events.onClick (TargetButtonClicked target.name)
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
