module Page.Writer exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , updatePageLinkClick
    , view
    )

import Appearance
import Calendar
import Data.Target exposing (Target)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Json.Encode as Encode
import Ports
import Skeleton
import State exposing (State)
import Time


type alias Model =
    { needsLocalSave : Bool
    , needsDbSave : Bool
    , timeSinceLastTouch : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { needsLocalSave = False
      , needsDbSave = False
      , timeSinceLastTouch = Time.millisToPosix 0
      }
    , Cmd.none
    )



-- Update


type Msg
    = WordsWritten String
    | SaveTimerTicked Time.Posix
    | SavedToDb


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        WordsWritten document ->
            updateCounts document model state
                |> (\( count, updatedModel, newState ) ->
                        ( { newState | additiveCount = count }
                        , ( { updatedModel | timeSinceLastTouch = state.currentTime }
                          , Cmd.none
                          )
                        )
                   )

        SaveTimerTicked time ->
            saveContentIfNeeded model state
                |> (\( updatedModel, contentMsg ) -> ( contentMsg, saveToDbIfNeeded updatedModel state ))
                |> (\( contentMsg, ( updatedModel, dbMsg ) ) ->
                        ( state, ( updatedModel, Cmd.batch [ contentMsg, dbMsg ] ) )
                   )

        SavedToDb ->
            ( state, ( model, Cmd.none ) )


saveContentIfNeeded : Model -> State -> ( Model, Cmd msg )
saveContentIfNeeded model state =
    if model.needsLocalSave then
        ( { model | needsLocalSave = False }, saveContent state )

    else
        ( model, Cmd.none )


saveContent : State -> Cmd msg
saveContent state =
    Ports.sendMessageWithJustContent Ports.SaveContent (State.encodeSaveState state)


saveToDbIfNeeded : Model -> State -> ( Model, Cmd msg )
saveToDbIfNeeded model state =
    if
        model.needsDbSave
            && ((Time.posixToMillis state.currentTime - Time.posixToMillis model.timeSinceLastTouch) > 5000)
    then
        ( { model | needsDbSave = False }, saveToDb state )

    else
        ( model, Cmd.none )


saveToDb : State -> Cmd msg
saveToDb state =
    case state.user of
        Just user ->
            Ports.sendMessageWithContentAndResponse
                Ports.SaveToDbSubcollection
                (posixToDate ( state.timeZone, state.currentTime )
                    |> encodeWordcountSave user.uid state.additiveCount
                )
                Ports.WriterDataSaved

        Nothing ->
            Cmd.none


updatePageLinkClick : State -> Cmd msg
updatePageLinkClick state =
    Cmd.batch [ saveContent state, saveToDb state ]



-- counting


updateCounts : String -> Model -> State -> ( Int, Model, State )
updateCounts document model state =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document

        dif : Int
        dif =
            trimmedWordCount - state.actualCount
    in
    ( updateWrittenCount state.additiveCount trimmedWordCount state dif
    , { model
        | needsLocalSave = True
        , needsDbSave = True
      }
    , { state
        | currentText = document
        , actualCount = trimmedWordCount
        , winProgress = calculateProgress state dif
      }
    )


updateWrittenCount : Int -> Int -> State -> Int -> Int
updateWrittenCount writtenCount trimmedWordCount state dif =
    if dif > 0 then
        writtenCount + dif

    else if state.settings.countMethod == State.Additive then
        writtenCount

    else
        trimmedWordCount


calculateProgress : State -> Int -> Int
calculateProgress state dif =
    case state.currentTarget of
        Just target ->
            if dif > 0 && state.ended == State.No then
                if state.winProgress + dif >= target.count then
                    target.count

                else
                    state.winProgress + dif

            else
                state.winProgress

        Nothing ->
            -1


countWords : String -> Int
countWords document =
    document
        |> String.replace "—" " "
        |> String.replace "–" " "
        |> String.words
        |> List.filter (String.any Char.isAlphaNum)
        |> List.length



-- view


view : Model -> State -> Skeleton.PageData Msg
view model state =
    { title = "Writing page"
    , headerSettings = Just (getHeaderSettings state)
    , body = showBody model state
    }


getHeaderSettings : State -> Skeleton.HeaderSettings
getHeaderSettings state =
    { actionButtonSettings =
        Just { action = "/target", label = "TARGET" }
    }


showBody : Model -> State -> Element Msg
showBody model state =
    column
        [ width fill
        , height fill
        ]
        [ case state.currentTarget of
            Nothing ->
                none

            Just currentTarget ->
                showProgressBar model state currentTarget
        , Input.multiline
            [ width fill
            , height fill
            , padding 25
            , Border.width 0
            , Border.rounded 0
            , htmlAttribute <| Html.Attributes.placeholder "Tap target to select one, then write your words here!"
            ]
            { onChange = WordsWritten
            , text = state.currentText
            , placeholder = Nothing
            , label = Input.labelHidden ""
            , spellcheck = True
            }
        ]


showProgressBar : Model -> State -> Target -> Element Msg
showProgressBar model state target =
    row
        [ width fill
        , height <| px 38
        , centerY
        , Background.color Appearance.siteBackgroundLight
        , inFront <|
            image
                [ width <| px 35
                , centerX
                , onLeft <|
                    el
                        [ alignRight, centerY ]
                    <|
                        text (target.name ++ "  ")
                , onRight <|
                    row [ centerY ]
                        [ el
                            [ alignLeft
                            , width shrink
                            ]
                          <|
                            text (currentTargetCounts state.winProgress target.count)
                        , el
                            [ alignLeft
                            ]
                          <|
                            text (currentTargetFightStatus model state)
                        ]
                ]
                { src = target.portraitSource
                , description = target.name ++ " portrait"
                }
        ]
        [ el
            [ width <| fillPortion state.winProgress
            , height <| px 20
            , Background.color Appearance.progressBarForeground
            ]
            none
        , el
            [ width <| fillPortion <| target.count - state.winProgress
            , height <| px 20
            , Background.color Appearance.progressBarBackground
            ]
            none
        ]


currentTargetCounts : Int -> Int -> String
currentTargetCounts winProgress winCount =
    "  "
        ++ String.fromInt winProgress
        ++ " / "
        ++ String.fromInt winCount


currentTargetFightStatus : Model -> State -> String
currentTargetFightStatus model state =
    "  "
        ++ (if state.ended /= State.No then
                State.endReasonToString state.ended

            else
                formatSecondsToString state.currentTargetTimerInSecs
           )


formatSecondsToString : Int -> String
formatSecondsToString seconds =
    formatSecondsToStringHourCheck seconds False


formatSecondsToStringHourCheck : Int -> Bool -> String
formatSecondsToStringHourCheck seconds hasHour =
    if (seconds < 3600 && seconds >= 60) || hasHour then
        String.padLeft 2 '0' (String.fromInt (seconds // 60))
            ++ ":"
            ++ formatSecondsToStringHourCheck (remainderBy 60 seconds) False

    else if seconds < 60 then
        String.padLeft 2 '0' (String.fromInt seconds)

    else
        String.fromInt (seconds // 3600)
            ++ ":"
            ++ formatSecondsToStringHourCheck (remainderBy 3600 seconds) True



-- Encoding


encodeWordcountSave : String -> Int -> Calendar.Date -> Encode.Value
encodeWordcountSave userId count date =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "doc", Encode.string userId )
        , ( "subcollection", Encode.string "days" )
        , ( "subdoc", Encode.string <| dateToSortableString date )
        , ( "data", encodeWordCount date count )
        ]


encodeWordCount : Calendar.Date -> Int -> Encode.Value
encodeWordCount date count =
    Encode.object
        [ ( "count", Encode.int count )
        , ( "date", Encode.int <| Calendar.toMillis date )
        ]


dateToSortableString : Calendar.Date -> String
dateToSortableString date =
    (String.fromInt <| Calendar.getYear date)
        ++ (String.padLeft 2 '0' <| String.fromInt <| Calendar.monthToInt <| Calendar.getMonth date)
        ++ (String.padLeft 2 '0' <| String.fromInt <| Calendar.getDay date)


posixToDate : ( Time.Zone, Time.Posix ) -> Calendar.Date
posixToDate ( zone, time ) =
    { year = Time.toYear zone time
    , month = Time.toMonth zone time
    , day = Time.toDay zone time
    }
        |> Calendar.fromRawParts
        |> Maybe.withDefault (Calendar.fromPosix time)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 SaveTimerTicked
