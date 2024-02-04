module Effect exposing (..)


type Effect
    = ShowMessage String
    | GameIsWon


withNone : a -> ( a, List Effect )
withNone a =
    ( a, [] )


andThen : (a -> ( a, List Effect )) -> ( a, List Effect ) -> ( a, List Effect )
andThen fun ( a, l ) =
    fun a
        |> Tuple.mapSecond ((++) l)
