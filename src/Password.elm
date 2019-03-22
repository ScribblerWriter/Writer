module Password exposing (Password, create, encode, getPassword)

import Json.Encode as Encode


type Password
    = Password String


create : String -> Password
create pass =
    Password pass



-- accessors


getPassword : Password -> String
getPassword (Password pass) =
    pass



-- encoding


encode : Password -> Encode.Value
encode (Password pass) =
    Encode.string pass
