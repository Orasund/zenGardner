module Effect exposing (..)


type Effect
    = ShowMessage String


withNone : a -> ( a, List Effect )
withNone a =
    ( a, [] )
