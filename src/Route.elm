module Route exposing (Route(..), fromUrl, toAbsolute)

import Error exposing (Error)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing (Parser, oneOf, s, top)


type Route
    = SignIn
    | SignUp
    | SignOut
    | Loading
    | Failure


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map SignIn top
        , Parser.map SignIn (s "signin")
        , Parser.map SignUp (s "signup")
        , Parser.map SignOut (s "signout")
        , Parser.map Loading (s "loading")
        , Parser.map Failure (s "failure")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Maybe Route -> String
routeToString route =
    case route of
        Just route_ ->
            case route_ of
                SignIn ->
                    "signin"

                SignUp ->
                    "signup"

                SignOut ->
                    "signout"

                Loading ->
                    "loading"

                Failure ->
                    "failure"

        Nothing ->
            "/"


toAbsolute : Maybe Route -> String
toAbsolute route =
    Url.Builder.absolute [ routeToString route ] []
