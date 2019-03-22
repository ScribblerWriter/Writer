module Uid exposing (Uid, create, getUid)


type Uid
    = Uid String


create : String -> Uid
create uid =
    Uid uid



-- accessors


getUid : Uid -> String
getUid (Uid uid) =
    uid
