module Config exposing (..)

{-| -}


{-| I experienced odd behaviour with dynamic screen-sizes on itch.io

That's why i tend to require width of 400-500px. Bigger screens just effect the margins.

This has also the added benefit, that the games are easier to support on mobile.

-}
screenMinWidth : number
screenMinWidth =
    400


screenMinHeight : number
screenMinHeight =
    400


cameraRadius : Int
cameraRadius =
    5


title : String
title =
    "Game Template"


tileSize : Int
tileSize =
    8 * 4


starsAmount =
    5
