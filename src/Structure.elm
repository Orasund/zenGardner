module Structure exposing (..)

import Dict exposing (Dict)
import Tile exposing (Obstacle(..), Tile(..))


fromEmojis : List String -> Dict ( Int, Int ) Tile
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
        |> Dict.fromList


fromEmoji : Char -> Maybe Tile
fromEmoji char =
    case char of
        '🪨' ->
            Just (ObstacleTile Stone)

        '🕋' ->
            Just (ObstacleTile Pole)

        '👣' ->
            Just Sand

        '🌱' ->
            Just Gras

        '🌳' ->
            Just Bonsai

        '❌' ->
            Nothing

        _ ->
            Nothing


startingArea : Dict ( Int, Int ) Tile
startingArea =
    [ "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌👣👣👣👣👣❌❌❌❌❌❌"
    , "❌❌❌❌👣👣🌱🌱🌱👣👣❌❌❌❌❌"
    , "❌❌❌❌👣🌱🌱🌱🌱🌱👣❌❌❌❌❌"
    , "❌❌❌❌👣🌱🌱🌱🌱🌱👣❌❌❌❌❌"
    , "❌❌❌❌👣👣🌱🌱🌱👣👣❌❌❌❌❌"
    , "❌❌❌❌❌👣👣👣👣👣❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis


grasArea : Dict ( Int, Int ) Tile
grasArea =
    [ "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌🌱🌱🌱🌱🌱❌❌❌❌❌❌❌"
    , "❌❌❌🌱🌱🌱🌱🌱🌱🌱🌱🌱❌❌❌❌"
    , "❌❌🌱🌱🌱🌱❌❌❌❌🌱🌱🌱❌❌❌"
    , "❌❌🌱🌱🌱❌❌❌❌❌❌🌱🌱🌱❌❌"
    , "❌❌🌱🌱🌱❌❌❌❌❌❌❌🌱🌱❌❌"
    , "❌❌❌🌱❌❌❌❌❌❌❌❌🌱🌱❌❌"
    , "❌❌❌🌱❌❌❌❌❌❌❌❌🌱🌱❌❌"
    , "❌❌❌🌱❌❌❌❌❌❌❌❌🌱🌱❌❌"
    , "❌❌❌🌱❌❌❌❌❌❌❌🌱🌱❌❌❌"
    , "❌❌🌱🌱❌❌❌❌❌❌🌱🌱🌱❌❌❌"
    , "❌❌🌱🌱🌱❌❌❌❌🌱🌱🌱❌❌❌❌"
    , "❌❌🌱🌱🌱🌱🌱🌱🌱🌱🌱❌❌❌❌❌"
    , "❌❌❌🌱🌱🌱🌱❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis


grasBatches : Dict ( Int, Int ) Tile
grasBatches =
    [ "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌🌱🌱🌱❌❌❌❌❌❌❌❌❌"
    , "❌❌❌🌱🌱🌱❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌🌱❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌🌱❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌🌱🌱❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌🌱🌱❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌🌱🌱🌱❌❌❌"
    , "❌❌❌🌱🌱❌❌❌❌❌❌🌱🌱❌❌❌"
    , "❌❌🌱🌱🌱🌱❌❌❌❌🌱🌱❌❌❌❌"
    , "❌❌🌱🌱❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌🌱🌱❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis


poleArea : Dict ( Int, Int ) Tile
poleArea =
    [ "👣👣👣👣👣👣👣👣👣❌❌❌❌❌❌❌"
    , "👣🕋👣🕋👣🕋👣🕋👣❌❌❌❌❌❌❌"
    , "👣👣👣👣👣👣👣👣👣❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌👣👣👣❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌👣🕋👣❌❌"
    , "❌👣👣👣❌❌❌❌❌❌❌👣👣👣❌❌"
    , "❌👣🕋👣❌🌱🌱🌱🌱❌❌👣🕋👣❌❌"
    , "❌👣👣👣❌🌱🕋🕋🌱❌❌👣👣👣❌❌"
    , "❌👣🕋👣❌🌱🕋🕋🌱❌❌👣🕋👣❌❌"
    , "❌👣👣👣❌🌱🌱🌱🌱❌❌👣👣👣❌❌"
    , "❌👣🕋👣❌❌❌❌❌❌❌👣🕋👣❌❌"
    , "❌👣👣👣❌❌❌❌❌❌❌👣👣👣❌❌"
    , "❌👣🕋👣❌❌👣👣👣👣👣👣👣👣👣❌"
    , "❌👣👣👣❌❌👣🕋👣🕋👣🕋👣🕋👣❌"
    , "❌❌❌❌❌❌👣👣👣👣👣👣👣👣👣❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis
        |> Dict.insert ( 6, 7 ) Statue
        |> Dict.insert ( 7, 7 ) SolidPlaceholder
        |> Dict.insert ( 6, 8 ) SolidPlaceholder
        |> Dict.insert ( 7, 8 ) SolidPlaceholder


shrineArea : Dict ( Int, Int ) Tile
shrineArea =
    [ "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌🕋🕋🕋🕋🕋🕋🕋🕋🕋🕋❌❌❌❌"
    , "❌❌🕋❌❌❌❌❌❌❌❌🕋❌❌❌❌"
    , "❌❌🕋❌❌❌❌❌❌❌❌🕋🕋🕋🕋🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌❌❌❌❌❌❌❌🕋"
    , "❌❌🕋❌❌❌❌❌🕋🕋❌❌❌❌❌🕋"
    , "❌❌🕋🕋🕋🕋🕋❌🕋🕋❌🕋❌🕋🕋🕋"
    , "❌❌❌❌❌❌❌❌🌱🌱❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis
        |> Dict.insert ( 8, 12 ) Statue
        |> Dict.insert ( 9, 12 ) SolidPlaceholder
        |> Dict.insert ( 8, 13 ) SolidPlaceholder
        |> Dict.insert ( 9, 13 ) SolidPlaceholder


stoneArea : Dict ( Int, Int ) Tile
stoneArea =
    [ "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌🪨🪨🪨🪨❌❌❌❌❌"
    , "❌❌❌❌🪨🪨❌❌❌❌❌🪨❌❌❌❌"
    , "❌❌❌🪨❌❌❌❌❌❌❌❌🪨❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌🪨❌❌❌"
    , "❌❌🪨❌❌❌❌❌❌❌❌❌❌🪨❌❌"
    , "❌❌🪨❌❌❌❌❌❌❌❌❌❌🪨❌❌"
    , "❌❌❌❌❌❌❌🌱🌱❌❌❌❌🪨❌❌"
    , "❌❌❌❌❌❌❌🌱🌱❌❌❌❌🪨❌❌"
    , "❌❌🪨❌❌❌❌❌❌❌❌❌🪨❌❌❌"
    , "❌❌🪨❌❌❌❌❌❌❌❌❌🪨❌❌❌"
    , "❌❌❌🪨❌❌❌❌❌❌❌❌🪨❌❌❌"
    , "❌❌❌🪨❌❌❌❌❌❌❌🪨❌❌❌❌"
    , "❌❌❌❌🪨❌❌❌❌🪨🪨❌❌❌❌❌"
    , "❌❌❌❌❌🪨🪨🪨🪨❌❌❌❌❌❌❌"
    , "❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌"
    ]
        |> fromEmojis


smallGarden : Dict ( Int, Int ) Tile
smallGarden =
    [ "❌❌❌❌❌👣👣👣❌❌❌❌❌❌❌❌"
    , "❌❌❌❌👣👣🌳👣❌❌❌❌❌❌❌❌"
    , "❌👣👣👣👣👣👣👣👣👣❌❌❌❌❌❌"
    , "❌👣🌳👣👣🌳👣👣👣👣👣👣❌❌❌❌"
    , "❌🪨🪨🪨👣👣👣👣👣👣🌳👣🪨🪨🪨❌"
    , "❌🪨❌👣🌳👣👣🌳👣👣👣👣👣❌🪨❌"
    , "❌🪨👣👣👣👣👣👣👣👣👣🌳👣❌❌❌"
    , "❌🪨👣🌳👣👣👣🌱🌱👣👣👣👣👣❌❌"
    , "❌❌👣👣👣👣🌱🌱🌱🌳👣👣🌳👣❌❌"
    , "❌❌❌👣🌳👣🌱👣👣👣👣👣👣👣👣👣"
    , "❌❌❌👣👣👣👣👣👣👣🌳👣👣👣🌳👣"
    , "❌🪨👣👣👣🌱👣👣🌳👣👣👣👣👣🪨👣"
    , "❌🪨👣🌳👣👣🌳👣👣👣👣🌳👣🌳🪨❌"
    , "❌🪨👣👣👣👣👣👣👣🌳👣👣👣👣🪨❌"
    , "❌🪨🪨🪨👣👣👣🌳👣👣👣❌👣🌳👣❌"
    , "❌❌❌❌❌❌👣👣👣❌❌❌👣👣👣❌"
    ]
        |> fromEmojis


transpose : ( Int, Int ) -> Dict ( Int, Int ) Tile -> List ( ( Int, Int ), Tile )
transpose ( x, y ) dict =
    dict
        |> Dict.toList
        |> List.map
            (Tuple.mapFirst
                (Tuple.mapBoth
                    ((+) x)
                    ((+) y)
                )
            )
