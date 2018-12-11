port module Main exposing (Model, main)

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


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { targetRowCount : Int
    , currentTargets : Dict Int Target
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetRowCount = 1
      , currentTargets = Dict.singleton 0 singleEmptyTarget
      }
    , Cmd.none
    )


singleEmptyTarget : Target
singleEmptyTarget =
    { name = ""
    , count = 0
    , minutes = 0
    , imgSource = ""
    , portraitSource = ""
    }


type TargetField
    = Name
    | Count
    | Minutes
    | ImgSource
    | PortraitSource


type Msg
    = FieldUpdated Int TargetField String
    | AddTargetButtonClicked
    | AddRowButtonClicked
    | LoadTargetsButtonClicked



-- Update


type alias Target =
    { name : String
    , count : Int
    , minutes : Int
    , imgSource : String
    , portraitSource : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldUpdated lineId field value ->
            ( { model
                | currentTargets = Dict.update lineId (updateTarget field value) model.currentTargets
              }
            , Cmd.none
            )

        AddRowButtonClicked ->
            ( { model
                | currentTargets =
                    Dict.insert (firstAvailableId model.currentTargets) singleEmptyTarget model.currentTargets
              }
            , Cmd.none
            )

        AddTargetButtonClicked ->
            ( model
            , outgoingMessage <|
                { operation = "SaveBatchToDb"
                , content = encodeTargetBatch <| Dict.values model.currentTargets
                }
            )

        LoadTargetsButtonClicked ->
            ( model
            , outgoingMessage <|
                { operation = "QueryDb"
                , content = encodeGetTargetsQuery
                }
            )


firstAvailableId : Dict k v -> Int
firstAvailableId dict =
    Dict.size dict + 1


updateTarget : TargetField -> String -> Maybe Target -> Maybe Target
updateTarget field value target =
    case target of
        Nothing ->
            Nothing

        Just toUpdate ->
            case field of
                Name ->
                    Just { toUpdate | name = value }

                Count ->
                    Just { toUpdate | count = Maybe.withDefault -1 (String.toInt value) }

                Minutes ->
                    Just { toUpdate | minutes = Maybe.withDefault -1 (String.toInt value) }

                ImgSource ->
                    Just { toUpdate | imgSource = value }

                PortraitSource ->
                    Just { toUpdate | portraitSource = value }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Message =
    { operation : String
    , content : JsonE.Value
    }


port outgoingMessage : Message -> Cmd msg


port incomingMessage : (Message -> msg) -> Sub msg


encodeTargetBatch : List Target -> JsonE.Value
encodeTargetBatch targets =
    JsonE.list encodeTargetSaveObject targets


encodeTargetSaveObject : Target -> JsonE.Value
encodeTargetSaveObject target =
    JsonE.object
        [ ( "collection", JsonE.string "targets" )
        , ( "id", JsonE.string target.name )
        , ( "data", encodeTarget target )
        ]


encodeTarget : Target -> JsonE.Value
encodeTarget target =
    JsonE.object
        [ ( "name", JsonE.string target.name )
        , ( "count", JsonE.int target.count )
        , ( "minutes", JsonE.int target.minutes )
        , ( "imgSource", JsonE.string target.imgSource )
        , ( "portraitSource", JsonE.string target.portraitSource )
        ]


encodeGetTargetsQuery : JsonE.Value
encodeGetTargetsQuery =
    JsonE.object
        [ ( "collection", JsonE.string "targets" ) ]



-- view


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 14
        , padding 10
        , width fill
        ]
    <|
        column
            []
            [ column
                []
              <|
                Dict.values (Dict.map buildTargetInputRow model.currentTargets)
            , row
                [ padding 5
                , width fill
                ]
                [ Input.button getButtonProperties
                    { onPress = Just AddTargetButtonClicked
                    , label = text "Add Targets"
                    }
                , Input.button getButtonProperties
                    { onPress = Just AddRowButtonClicked
                    , label = text "Add Row"
                    }
                , Input.button getButtonProperties
                    { onPress = Just LoadTargetsButtonClicked
                    , label = text "Load Targets"
                    }
                ]
            ]


getButtonProperties : List (Attribute Msg)
getButtonProperties =
    [ Border.rounded 2
    , Border.width 1
    , Border.shadow
        { offset = ( 1, 1 )
        , size = 1
        , blur = 0
        , color = rgb255 0 0 0
        }
    , padding 2
    ]


buildTargetInputRow : Int -> Target -> Element Msg
buildTargetInputRow rowId target =
    row
        [ width fill ]
        [ Input.text [ width <| fillPortion 3 ]
            { onChange = FieldUpdated rowId Name
            , text = target.name
            , placeholder = Just <| Input.placeholder [] (text "Name")
            , label = Input.labelHidden "Target name"
            }
        , Input.text [ width <| fillPortion 1 ]
            { onChange = FieldUpdated rowId Count
            , text =
                if target.count == 0 then
                    ""

                else
                    String.fromInt target.count
            , placeholder = Just <| Input.placeholder [] (text "Count")
            , label = Input.labelHidden "Word count"
            }
        , Input.text [ width <| fillPortion 1 ]
            { onChange = FieldUpdated rowId Minutes
            , text =
                if target.minutes == 0 then
                    ""

                else
                    String.fromInt target.minutes
            , placeholder = Just <| Input.placeholder [] (text "Minutes")
            , label = Input.labelHidden "Minutes to win"
            }
        , Input.text [ width <| fillPortion 5 ]
            { onChange = FieldUpdated rowId ImgSource
            , text = target.imgSource
            , placeholder = Just <| Input.placeholder [] (text "Image source")
            , label = Input.labelHidden "Selection image source"
            }
        , Input.text [ width <| fillPortion 5 ]
            { onChange = FieldUpdated rowId PortraitSource
            , text = target.portraitSource
            , placeholder = Just <| Input.placeholder [] (text "Portrait source")
            , label = Input.labelHidden "Portrait image source"
            }
        ]
