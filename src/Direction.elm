module Direction exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


rotClockwise : Direction -> Direction
rotClockwise direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


rotCounterclock : Direction -> Direction
rotCounterclock direction =
    case direction of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


toPoint : Direction -> ( Int, Int )
toPoint dir =
    case dir of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )
