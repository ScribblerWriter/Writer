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
import Data.Target exposing (Target)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Ports
import Skeleton
import State exposing (State)
import Time


type alias Model =
    { currentTargetTimerInSecs : Int
    , winProgress : Int
    , endMessage : String
    , touched : Bool
    }


type EndReason
    = TimeExpired
    | WordsReached


init : ( Model, Cmd Msg )
init =
    ( { currentTargetTimerInSecs = 0
      , winProgress = 0
      , endMessage = ""
      , touched = False
      }
    , Cmd.none
    )



-- Update


type Msg
    = WordsWritten String
    | SaveTimerTicked Time.Posix
    | TargetTimerTicked Time.Posix


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        WordsWritten document ->
            updateCounts document model state
                |> (\( count, updatedModel, newState ) ->
                        ( { newState | additiveCount = count }, ( updatedModel, Cmd.none ) )
                   )

        SaveTimerTicked _ ->
            if model.touched then
                ( state
                , ( { model | touched = False }
                  , saveContent model state
                  )
                )

            else
                ( state, ( model, Cmd.none ) )

        TargetTimerTicked _ ->
            case state.currentTarget of
                Nothing ->
                    ( state, ( model, Cmd.none ) )

                Just target ->
                    if target.new then
                        ( { state | currentTarget = Just { target | new = False } }
                        , ( { model | currentTargetTimerInSecs = target.minutes * 60 }, Cmd.none )
                        )

                    else if model.currentTargetTimerInSecs <= 0 then
                        ( state
                        , ( { model | endMessage = endFight model TimeExpired }, Cmd.none )
                        )

                    else
                        ( state
                        , ( { model | currentTargetTimerInSecs = model.currentTargetTimerInSecs - 1 }, Cmd.none )
                        )


saveContent : Model -> State -> Cmd msg
saveContent model state =
    Ports.sendMessageWithJustContent Ports.SaveContent (State.encodeSaveState state)


updatePageLinkClick : Model -> State -> Cmd msg
updatePageLinkClick model state =
    saveContent model state



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
        | winProgress = calculateProgress model state dif
        , endMessage = generateEndMessage model state dif
        , touched = True
      }
    , { state
        | currentText = document
        , actualCount = trimmedWordCount
      }
    )


updateWrittenCount : Int -> Int -> State -> Int -> Int
updateWrittenCount writtenCount trimmedWordCount state dif =
    if dif > 0 then
        writtenCount + dif

    else if state.countMethod == State.Additive then
        writtenCount

    else
        trimmedWordCount


calculateProgress : Model -> State -> Int -> Int
calculateProgress model state dif =
    case state.currentTarget of
        Just target ->
            if dif > 0 then
                if model.winProgress + dif >= target.count then
                    target.count

                else
                    model.winProgress + dif

            else
                model.winProgress

        Nothing ->
            0


generateEndMessage : Model -> State -> Int -> String
generateEndMessage model state dif =
    case state.currentTarget of
        Just target ->
            if dif > 0 then
                if model.winProgress + dif >= target.count then
                    endFight model WordsReached

                else
                    model.endMessage

            else
                model.endMessage

        Nothing ->
            ""


endFight : Model -> EndReason -> String
endFight model reason =
    case reason of
        TimeExpired ->
            "Time's up!"

        WordsReached ->
            "You win!"


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
    , signOutButtonSettings =
        Just { action = "/signout", label = "Sign Out" }
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
                showProgressBar model currentTarget
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


showProgressBar : Model -> Target -> Element Msg
showProgressBar model target =
    row
        [ width fill
        , height <| px 38
        , centerY
        , Background.color Appearance.siteWritingBackground
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
                            text (currentTargetCounts model.winProgress target.count)
                        , el
                            [ alignLeft
                            ]
                          <|
                            text (currentTargetFightStatus model)
                        ]
                ]
                { src = target.portraitSource
                , description = target.name ++ " portrait"
                }
        ]
        [ el
            [ width <| fillPortion model.winProgress
            , height <| px 20
            , Background.color Appearance.progressBarForeground
            ]
            none
        , el
            [ width <| fillPortion <| target.count - model.winProgress
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


currentTargetFightStatus : Model -> String
currentTargetFightStatus model =
    "  "
        ++ (if model.endMessage /= "" then
                model.endMessage

            else
                formatSecondsToString model.currentTargetTimerInSecs
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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 SaveTimerTicked
        , Time.every 1000 TargetTimerTicked
        ]
