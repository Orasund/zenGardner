module Tile exposing (..)

import Direction exposing (Direction)


type Tile
    = Stone
    | Path Direction Direction
    | Sand
    | Gras
    | Water
    | Sign String


isSolid : Tile -> Bool
isSolid tile =
    case tile of
        Stone ->
            True

        Path _ _ ->
            True

        Sand ->
            False

        Gras ->
            False

        Water ->
            False

        Sign _ ->
            True
