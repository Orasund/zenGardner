module Structure exposing (..)

import Tile exposing (Tile(..))


fromEmojis : List String -> List ( ( Int, Int ), Tile )
fromEmojis rows =
    rows
        |> List.indexedMap
            (\y row ->
                row
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            char
                                |> fromEmoji
                                |> Maybe.map (Tuple.pair ( x, y ))
                        )
                    |> List.filterMap identity
            )
        |> List.concat


fromEmoji : Char -> Maybe Tile
fromEmoji char =
    case char of
        '🪨' ->
            Just Stone

        '👣' ->
            Just Sand

        '🌱' ->
            Just Gras

        '❌' ->
            Nothing

        _ ->
            Nothing


startingArea : List ( ( Int, Int ), Tile )
startingArea =
    [ "❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌"
    , "❌❌🌱🌱🌱❌❌❌"
    , "❌🌱🌱🌱🌱🌱❌❌"
    , "❌🌱🌱🌱🌱🌱❌❌"
    , "❌❌🌱🌱🌱❌❌❌"
    , "❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis


transpose : ( Int, Int ) -> List ( ( Int, Int ), Tile ) -> List ( ( Int, Int ), Tile )
transpose ( x, y ) =
    List.map
        (Tuple.mapFirst
            (Tuple.mapBoth
                ((+) x)
                ((+) y)
            )
        )
