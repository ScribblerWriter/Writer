module Page.Writer exposing (Model, Msg, init, update, view)

import Appearance
import Data.Target exposing (Target)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Skeleton
import State exposing (State)


type alias Model =
    { currentTarget : Maybe Target
    , currentTargetTimerInSecs : Int
    , currentText : String
    , winProgress : Int
    , endMessage : String
    , actualWordsAtLastCheck : Int
    , touched : Bool
    , countMethod : CountMethod
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTarget = Nothing
      , currentTargetTimerInSecs = 0
      , currentText = ""
      , winProgress = 0
      , endMessage = ""
      , actualWordsAtLastCheck = 0
      , touched = False
      , countMethod = Additive
      }
    , Cmd.none
    )


type CountMethod
    = Additive
    | Subtractive


type Msg
    = WordsWritten String


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        WordsWritten document ->
            updateCounts state.writtenCount document model
                |> (\( count, updatedModel ) ->
                        ( { state | writtenCount = count }
                        , ( updatedModel, Cmd.none )
                        )
                   )


updateCounts : Int -> String -> Model -> ( Int, Model )
updateCounts writtenCount document model =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document

        dif : Int
        dif =
            trimmedWordCount - model.actualWordsAtLastCheck
    in
    ( if dif > 0 then
        writtenCount + dif

      else if model.countMethod == Additive then
        writtenCount

      else
        trimmedWordCount
    , { model
        | actualWordsAtLastCheck = trimmedWordCount
        , winProgress =
            case model.currentTarget of
                Just target ->
                    if dif > 0 then
                        if model.winProgress + dif >= target.winCount then
                            target.winCount

                        else
                            model.winProgress + dif

                    else
                        model.winProgress

                Nothing ->
                    0
        , endMessage =
            case model.currentTarget of
                Just target ->
                    if dif > 0 then
                        if model.winProgress + dif >= target.winCount then
                            endFight model WordsReached

                        else
                            model.endMessage

                    else
                        model.endMessage

                Nothing ->
                    ""
        , currentText = document
        , touched = True
      }
    )


type EndReason
    = TimeExpired
    | WordsReached


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


view : State -> Model -> Skeleton.PageData Msg
view state model =
    { title = "Writing page"
    , headerSettings =
        Just
            { writtenCount = state.writtenCount
            , actionButtonSettings =
                Just { action = "/target", label = "TARGET" }
            , signOutButtonSettings =
                Just { action = "/signout", label = "Sign Out" }
            }
    , body =
        column
            [ width fill
            , height fill
            ]
            [ case model.currentTarget of
                Nothing ->
                    none

                Just target ->
                    showProgressBar model target
            , Input.multiline
                [ width fill
                , height fill
                , padding 25
                , Border.width 0
                , Border.rounded 0
                , htmlAttribute <| Html.Attributes.placeholder "Tap target to select one, then write your words here!"
                ]
                { onChange = WordsWritten
                , text = model.currentText
                , placeholder = Nothing
                , label = Input.labelHidden ""
                , spellcheck = True
                }
            ]
    }


showProgressBar : Model -> Target -> Element Msg
showProgressBar model target =
    row
        [ width fill
        , height <| px 38
        , centerY
        , inFront <|
            image
                [ width <| px 35
                , centerX
                , onLeft <|
                    el
                        [ alignRight, centerY ]
                    <|
                        text (currentTargetName target.name)
                , onRight <|
                    row [ centerY ]
                        [ el
                            [ alignLeft
                            , width shrink
                            ]
                          <|
                            text (currentTargetCounts model.winProgress target.winCount)
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
            [ width <| fillPortion <| target.winCount - model.winProgress
            , height <| px 20
            , Background.color Appearance.progressBarBackground
            ]
            none
        ]


currentTargetName : String -> String
currentTargetName name =
    name ++ "  "


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
    if seconds < 60 then
        String.padLeft 2 '0' (String.fromInt seconds)

    else if seconds < 3600 then
        String.padLeft 2 '0' (String.fromInt (seconds // 60))
            ++ ":"
            ++ formatSecondsToString (remainderBy 60 seconds)

    else
        String.fromInt (seconds // 3600)
            ++ ":"
            ++ formatSecondsToString (remainderBy 3600 seconds)
