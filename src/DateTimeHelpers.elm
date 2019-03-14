module DateTimeHelpers exposing (dateToSortableString, posixToDate)

import Calendar
import Time


dateToSortableString : Calendar.Date -> String
dateToSortableString date =
    (String.fromInt <| Calendar.getYear date)
        ++ (String.padLeft 2 '0' <| String.fromInt <| Calendar.monthToInt <| Calendar.getMonth date)
        ++ (String.padLeft 2 '0' <| String.fromInt <| Calendar.getDay date)


posixToDate : ( Time.Zone, Time.Posix ) -> Calendar.Date
posixToDate ( zone, time ) =
    { year = Time.toYear zone time
    , month = Time.toMonth zone time
    , day = Time.toDay zone time
    }
        |> Calendar.fromRawParts
        |> Maybe.withDefault (Calendar.fromPosix time)
