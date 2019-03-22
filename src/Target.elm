module Target exposing (Target)


type alias Target =
    { name : String
    , imgSource : String
    , portraitSource : String
    , count : Int
    , minutes : Int
    , new : Bool
    }
