module Page.TargetSelector exposing (Model, Msg, init, subscriptions, update, view)

import Appearance
import Browser.Navigation as Nav
import Data.Target exposing (Target)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Ports
import Skeleton
import State exposing (State)
import Url.Builder


type alias Model =
    { targets : Dict String Target }


init : ( Model, Cmd Msg )
init =
    ( { targets = Dict.empty }
    , Ports.sendMessageWithContentAndResponse
        Ports.QueryDbMultiple
        encodeGetTargetsQuery
        Ports.TargetListReturned
    )



-- Update


type Msg
    = TargetButtonClicked Target
    | MessageReceived Ports.InMessage


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        TargetButtonClicked target ->
            ( { state | currentTarget = Just target }
            , ( model, Nav.pushUrl state.key (Url.Builder.absolute [] []) )
            )

        MessageReceived message ->
            case Ports.stringToInOperation message.operation of
                Ports.TargetListReturned ->
                    case message.content of
                        Just content ->
                            ( state
                            , ( { model | targets = decodeTargets content }, Cmd.none )
                            )

                        Nothing ->
                            ( state, ( model, Cmd.none ) )

                _ ->
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
    { actionButtonSettings =
        Just { action = "/", label = "CANCEL" }
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
        [ Background.color Appearance.siteBackgroundTargetSelection
        , width fill
        , height fill
        , padding 25
        , scrollbarY
        ]
    <|
        buildTargetRows imagesPerRow imageWidth (Dict.values model.targets)


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
        [ Events.onClick (TargetButtonClicked target)
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
            , el [ centerX ] <| text <| String.fromInt target.count ++ " words"
            , el [ centerX ] <| text <| String.fromInt target.minutes ++ " minutes"
            , el [ centerX ] <| text <| String.fromInt (target.count // target.minutes) ++ " wpm"
            ]



-- Queries


encodeGetTargetsQuery : Encode.Value
encodeGetTargetsQuery =
    Encode.object
        [ ( "collection", Encode.string "targets" ) ]



-- Decoding


decodeTargets : Decode.Value -> Dict String Target
decodeTargets targetsValue =
    case Decode.decodeValue (Decode.list targetDecoder) targetsValue of
        Ok targets ->
            List.foldl (\target -> Dict.insert target.name target) Dict.empty targets

        Err _ ->
            Dict.empty


targetDecoder : Decode.Decoder Target
targetDecoder =
    Decode.map6 Target
        (Decode.field "name" Decode.string)
        (Decode.field "imgSource" Decode.string)
        (Decode.field "portraitSource" Decode.string)
        (Decode.field "count" Decode.int)
        (Decode.field "minutes" Decode.int)
        (Decode.succeed True)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incomingMessage MessageReceived ]
