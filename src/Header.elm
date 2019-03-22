module Header exposing (Header, create, view)

import ActionButton exposing (ActionButton)
import Element exposing (..)


type Header
    = Header (Maybe ActionButton)


create : Maybe ActionButton -> Header
create button =
    Header button



-- view


view : Element msg
view =
    none
