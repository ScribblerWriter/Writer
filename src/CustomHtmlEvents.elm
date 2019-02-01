module CustomHtmlEvents exposing (onEnter)

import Element exposing (Attribute, htmlAttribute)
import Html.Events exposing (on)
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute <| on "keydown" (enterPressedDecoder msg)


enterPressedDecoder : msg -> Decode.Decoder msg
enterPressedDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "Enter" ->
                        Decode.succeed msg

                    _ ->
                        Decode.fail "Not Enter"
            )
