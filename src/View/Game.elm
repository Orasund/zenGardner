module View.Game exposing (..)

import Config
import Dict
import Game exposing (Game)
import Html exposing (Html)
import Layout
import Sprite.Big
import Sprite.Small
import Tile exposing (Tile(..))
import View.Sprite as Sprite


small : Game -> Html msg
small game =
    let
        ( playerX, playerY ) =
            game.playerPos

        size =
            (Config.cameraRadius + 1) * 4
    in
    List.range (playerY - size)
        (playerY + size)
        |> List.map
            (\y ->
                List.range (playerX - size)
                    (playerX + size)
                    |> List.map
                        (\x ->
                            viewSmallTile ( x, y ) game
                        )
                    |> Layout.row []
            )
        |> Layout.column []


toHtml : Game -> Html msg
toHtml game =
    let
        ( playerX, playerY ) =
            game.playerPos
    in
    List.range (playerY - Config.cameraRadius)
        (playerY + Config.cameraRadius)
        |> List.map
            (\y ->
                List.range (playerX - Config.cameraRadius)
                    (playerX + Config.cameraRadius)
                    |> List.map
                        (\x ->
                            viewBigTile ( x, y ) game
                        )
                    |> Layout.row []
            )
        |> Layout.column []


viewSmallTile : ( Int, Int ) -> Game -> Html msg
viewSmallTile ( x, y ) game =
    Dict.get ( x, y ) game.tiles
        |> Maybe.map Sprite.Small.fromTile
        |> Maybe.withDefault []
        |> Sprite.viewSmallSprite []


viewBigTile : ( Int, Int ) -> Game -> Html msg
viewBigTile ( x, y ) game =
    if game.playerPos == ( x, y ) then
        Sprite.Big.viewPlayer
            |> Sprite.viewSprite []

    else
        case Dict.get ( x, y ) game.tiles of
            Just Stone ->
                Sprite.Big.viewStone
                    |> Sprite.viewSprite []

            Just (Path dir1 dir2) ->
                Sprite.viewPath [] dir1 dir2

            Just Sand ->
                Sprite.Big.viewSand
                    |> Sprite.viewSprite []

            Just Gras ->
                Sprite.Big.viewGras
                    |> Sprite.viewSprite []

            Just Water ->
                Sprite.Big.viewWater
                    |> Sprite.viewSprite []

            Just (Sign _) ->
                Sprite.Big.sign
                    |> Sprite.viewSprite []

            Nothing ->
                Sprite.Big.viewEmpty
                    |> Sprite.viewSprite []
