module Tile exposing (..)

import Direction exposing (Direction)


type Tile
    = ObstacleTile Obstacle
    | Path Direction Direction
    | Sand
    | Gras
    | Water
    | Sign String
    | Bonsai
    | Statue
    | Shrine
    | Star
    | SolidPlaceholder


type Obstacle
    = Stone
    | Pole


isSolid : Tile -> Bool
isSolid tile =
    case tile of
        ObstacleTile _ ->
            True

        Bonsai ->
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

        Statue ->
            True

        Shrine ->
            True

        Star ->
            True

        SolidPlaceholder ->
            True
