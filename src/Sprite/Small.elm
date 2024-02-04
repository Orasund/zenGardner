module Sprite.Small exposing (..)

import Direction exposing (Direction(..))
import Tile exposing (Tile(..))


path : Direction -> Direction -> List String
path dir1 dir2 =
    case ( dir1, dir2 ) of
        ( Up, Down ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]

        ( Down, Up ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]

        ( Up, Up ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]

        ( Down, Down ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]

        ( Left, Right ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Right, Left ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Left, Left ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Right, Right ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Up, Left ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Left, Up ) ->
            [ "🟨🟨"
            , "⬜⬜"
            ]

        ( Up, Right ) ->
            [ "⬜🟨"
            , "⬜⬜"
            ]

        ( Right, Up ) ->
            [ "⬜🟨"
            , "⬜⬜"
            ]

        ( Down, Left ) ->
            [ "🟨🟨"
            , "⬜🟨"
            ]

        ( Left, Down ) ->
            [ "🟨🟨"
            , "⬜🟨"
            ]

        ( Down, Right ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]

        ( Right, Down ) ->
            [ "⬜🟨"
            , "⬜🟨"
            ]
