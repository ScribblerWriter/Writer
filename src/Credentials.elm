module Credentials exposing (Credentials, create, encode)

import Email exposing (Email)
import Json.Encode as Encode
import Password exposing (Password)


type Credentials
    = Credentials
        { email : Email
        , password : Password
        }


create : Email -> Password -> Credentials
create email pass =
    Credentials { email = email, password = pass }



-- encoding


encode : Credentials -> Encode.Value
encode (Credentials creds) =
    Encode.object
        [ ( "email", Email.encode creds.email )
        , ( "pass", Password.encode creds.password )
        ]
