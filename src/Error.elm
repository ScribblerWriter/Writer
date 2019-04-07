module Error exposing (Error, combine, create, getErrors)

import ErrorEntry exposing (ErrorEntry)


type Error
    = Error (List ErrorEntry)


create : String -> String -> Error
create message details =
    Error [ ErrorEntry.create message details ]



-- accessors


getErrors : Error -> List ErrorEntry
getErrors (Error errors) =
    errors


combine : Error -> Error -> Error
combine (Error list1) (Error list2) =
    Error (List.append list1 list2)
