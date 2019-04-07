module Link exposing (Destination(..), getAbsolute, getUrl)

import Url.Builder


type Destination
    = SignIn
    | SignUp
    | SignOut
    | Home
    | Settings
    | Dashboard
    | Target



-- accessors


getUrl : Destination -> String
getUrl dest =
    case dest of
        Home ->
            "/"

        SignIn ->
            "signin"

        SignUp ->
            "signup"

        SignOut ->
            "signout"

        Settings ->
            "settings"

        Dashboard ->
            "dashboard"

        Target ->
            "target"


getAbsolute : Destination -> String
getAbsolute dest =
    Url.Builder.absolute [ getUrl dest ] []
