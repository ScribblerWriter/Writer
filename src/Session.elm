module Session exposing
    ( Session
    , create
    , getKey
    , getLastUrl
    , getWindowDimensions
    , mapLastUrl
    , mapWindowDimensions
    )

import Browser.Navigation exposing (Key)
import Dimensions exposing (Dimensions)
import Json.Decode exposing (Value)
import Route exposing (Route)
import Url exposing (Url)


type Session
    = Session
        { key : Key
        , windowDimensions : Dimensions
        , lastRequestedUrl : Maybe Route
        }


create : Value -> Url -> Key -> Session
create flags url key =
    Session
        { key = key
        , windowDimensions = Dimensions.decode flags
        , lastRequestedUrl = Route.fromUrl url
        }



-- accessors


getWindowDimensions : Session -> Dimensions
getWindowDimensions (Session { windowDimensions }) =
    windowDimensions


mapWindowDimensions : (Dimensions -> Dimensions) -> Session -> Session
mapWindowDimensions mapper (Session session) =
    mapper session.windowDimensions
        |> (\newDims -> Session { session | windowDimensions = newDims })


getLastUrl : Session -> Maybe Route
getLastUrl (Session { lastRequestedUrl }) =
    lastRequestedUrl


mapLastUrl : (Maybe Route -> Maybe Route) -> Session -> Session
mapLastUrl mapper (Session session) =
    mapper session.lastRequestedUrl
        |> (\newUrl -> Session { session | lastRequestedUrl = newUrl })


getKey : Session -> Key
getKey (Session { key }) =
    key
