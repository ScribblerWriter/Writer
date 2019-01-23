module Page.Writer exposing (Model, Msg(..), init, subscriptions, update, updatePageLinkClick, view)

import Appearance
import Data.Target exposing (Target)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Skeleton
import State exposing (State)
import Time


type alias Model =
    { currentTargetTimerInSecs : Int
    , currentText : String
    , winProgress : Int
    , endMessage : String
    , actualWordsAtLastCheck : Int
    , touched : Bool
    , countMethod : CountMethod
    }


type EndReason
    = TimeExpired
    | WordsReached


init : ( Model, Cmd Msg )
init =
    ( { currentTargetTimerInSecs = 0
      , currentText = ""
      , winProgress = 0
      , endMessage = ""
      , actualWordsAtLastCheck = 0
      , touched = False
      , countMethod = Additive
      }
    , Ports.sendMessageWithJustResponse Ports.LoadContent Ports.ContentLoaded
    )


type CountMethod
    = Additive
    | Subtractive



-- Update


type Msg
    = WordsWritten String
    | SaveTimerTicked Time.Posix
    | MessageReceived Ports.InMessage
    | TargetTimerTicked Time.Posix


update : Msg -> Model -> State -> ( State, ( Model, Cmd msg ) )
update msg model state =
    case msg of
        WordsWritten document ->
            updateCounts document model state
                |> (\( count, updatedModel ) ->
                        ( { state | writtenCount = count }, ( updatedModel, Cmd.none ) )
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

        MessageReceived message ->
            case Ports.stringToInOperation message.operation of
                Ports.ContentLoaded ->
                    updateContent model state message.content

                _ ->
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
    Ports.sendMessageWithJustContent Ports.SaveContent (encodeSaveObject model state)


updateContent : Model -> State -> Maybe Encode.Value -> ( State, ( Model, Cmd msg ) )
updateContent model state content =
    case content of
        Just data ->
            ( { state | writtenCount = getValue wordCountDecoder data 0 }
            , ( { model
                    | currentText = getValue textDecoder data ""
                    , countMethod = getValue methodDecoder data Additive
                    , actualWordsAtLastCheck = getValue actualCountDecoder data 0
                }
              , Cmd.none
              )
            )

        Nothing ->
            ( state, ( model, Cmd.none ) )


updatePageLinkClick : Model -> State -> Cmd msg
updatePageLinkClick model state =
    saveContent model state



-- counting


updateCounts : String -> Model -> State -> ( Int, Model )
updateCounts document model state =
    let
        trimmedWordCount : Int
        trimmedWordCount =
            countWords document

        dif : Int
        dif =
            trimmedWordCount - model.actualWordsAtLastCheck
    in
    ( updateWrittenCount state.writtenCount trimmedWordCount model dif
    , { model
        | actualWordsAtLastCheck = trimmedWordCount
        , winProgress = calculateProgress model state dif
        , endMessage = generateEndMessage model state dif
        , currentText = document
        , touched = True
      }
    )


updateWrittenCount : Int -> Int -> Model -> Int -> Int
updateWrittenCount writtenCount trimmedWordCount model dif =
    if dif > 0 then
        writtenCount + dif

    else if model.countMethod == Additive then
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
    { writtenCount = state.writtenCount
    , actionButtonSettings =
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
            , text = model.currentText
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



-- Encoding / Decoding
--{ count : Int
--, text : String
--, actualCount : Int
--, method : CountMethod } -- "additive"/"subtractive"


getValue : Decode.Decoder a -> Encode.Value -> a -> a
getValue decoder string errorVal =
    case Decode.decodeValue decoder string of
        Err _ ->
            errorVal

        Ok value ->
            value


wordCountDecoder : Decode.Decoder Int
wordCountDecoder =
    Decode.field "count" Decode.int


textDecoder : Decode.Decoder String
textDecoder =
    Decode.field "text" Decode.string


actualCountDecoder : Decode.Decoder Int
actualCountDecoder =
    Decode.field "actualCount" Decode.int


methodDecoder : Decode.Decoder CountMethod
methodDecoder =
    Decode.field "method" Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "additive" ->
                        Decode.succeed Additive

                    "subtractive" ->
                        Decode.succeed Subtractive

                    wrongValue ->
                        Decode.fail ("Count method decoding failed. Value: " ++ wrongValue)
            )



-- Encoding


encodeSaveObject : Model -> State -> Encode.Value
encodeSaveObject model state =
    Encode.object
        [ ( "count", Encode.int state.writtenCount )
        , ( "text", Encode.string model.currentText )
        , ( "method", methodEncoder model.countMethod )
        , ( "actualCount", Encode.int model.actualWordsAtLastCheck )
        ]


methodEncoder : CountMethod -> Encode.Value
methodEncoder method =
    case method of
        Additive ->
            Encode.string "additive"

        Subtractive ->
            Encode.string "subtractive"



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incomingMessage MessageReceived
        , Time.every 1000 SaveTimerTicked
        , Time.every 1000 TargetTimerTicked
        ]
