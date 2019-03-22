module ActionButton exposing
    ( Action(..)
    , ActionButton
    , create
    , getActionUrl
    , getLabel
    )


type ActionButton
    = ActionButton Action


type Action
    = Target
    | Cancel
    | GiveUp


create : Action -> ActionButton
create action =
    ActionButton action



-- accessors


getActionUrl : ActionButton -> String
getActionUrl (ActionButton action) =
    case action of
        Target ->
            "/target"

        Cancel ->
            "/"

        GiveUp ->
            "/"


getLabel : ActionButton -> String
getLabel (ActionButton action) =
    case action of
        Target ->
            "TARGET"

        Cancel ->
            "CANCEL"

        GiveUp ->
            "GIVE UP"
