module User exposing (User, generator, getEmail, getUid)

import Email exposing (..)
import Uid exposing (..)


type User
    = User
        { email : Email
        , uid : Uid
        }


generator : Email -> Uid -> User
generator email uid =
    User { email = email, uid = uid }



-- accessors


getEmail : User -> Email
getEmail (User { email }) =
    email


getUid : User -> Uid
getUid (User { uid }) =
    uid
