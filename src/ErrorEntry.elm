module ErrorEntry exposing (ErrorEntry, create, getDetails, getMessage)


type ErrorEntry
    = ErrorEntry
        { message : String
        , details : String
        }


create : String -> String -> ErrorEntry
create message details =
    ErrorEntry { message = message, details = details }



-- accessors


getMessage : ErrorEntry -> String
getMessage (ErrorEntry { message }) =
    message


getDetails : ErrorEntry -> String
getDetails (ErrorEntry { details }) =
    details
