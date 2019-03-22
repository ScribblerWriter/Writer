module Email exposing (Email, create, encode, getEmail)

import Json.Encode as Encode


type Email
    = Email String


create : String -> Email
create email =
    Email email



-- accessors


getEmail : Email -> String
getEmail (Email email) =
    email



-- encoding


encode : Email -> Encode.Value
encode (Email email) =
    Encode.string email
