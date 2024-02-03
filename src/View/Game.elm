module View.Game exposing (..)

import Config
import Dict
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Style
import Layout
import Sprite.Big
import Sprite.Small
import Tile exposing (Obstacle(..), Tile(..))
import View.Sprite as Sprite


toHtml : Game -> Html msg
toHtml game =
    let
        ( playerX, playerY ) =
            game.playerPos
    in
    List.range (playerY - Config.cameraRadius)
        (playerY + Config.cameraRadius)
        |> List.concatMap
            (\y ->
                List.range (playerX - Config.cameraRadius)
                    (playerX + Config.cameraRadius)
                    |> List.map
                        (\x ->
                            viewBigTile
                                [ Html.Style.positionAbsolute
                                , Html.Style.left (String.fromInt ((x + Config.cameraRadius - playerX) * Config.tileSize) ++ "px")
                                , Html.Style.top (String.fromInt ((y + Config.cameraRadius - playerY) * Config.tileSize) ++ "px")
                                ]
                                ( x, y )
                                game
                        )
            )
        |> Html.div [ Html.Style.positionRelative ]


viewSmallTile : ( Int, Int ) -> Game -> Html msg
viewSmallTile ( x, y ) game =
    Dict.get ( x, y ) game.tiles
        |> Maybe.map Sprite.Small.fromTile
        |> Maybe.withDefault []
        |> Sprite.viewSmallSprite []


viewBigTile : List (Attribute msg) -> ( Int, Int ) -> Game -> Html msg
viewBigTile attrs ( x, y ) game =
    if game.playerPos == ( x, y ) then
        Sprite.Big.viewPlayer
            |> Sprite.viewSprite attrs

    else
        case Dict.get ( x, y ) game.tiles of
            Just (ObstacleTile obstacle) ->
                case obstacle of
                    Stone ->
                        Sprite.Big.stone
                            |> Sprite.viewSprite attrs

                    Pole ->
                        Sprite.Big.pole
                            |> Sprite.viewSprite attrs

            Just Statue ->
                Sprite.Big.statue
                    |> Sprite.viewLargeSprite attrs

            Just Shrine ->
                Sprite.Big.shrine
                    |> Sprite.viewLargeSprite attrs

            Just Bonsai ->
                [ ( x - 1, y )
                , ( x - 1, y - 1 )
                , ( x - 1, y + 1 )
                , ( x + 1, y )
                , ( x + 1, y - 1 )
                , ( x + 1, y + 1 )
                , ( x, y - 1 )
                , ( x, y + 1 )
                ]
                    |> List.all
                        (\pos ->
                            Dict.get pos game.tiles
                                |> (/=) (Just Sand)
                        )
                    |> (\surounded ->
                            if surounded then
                                Sprite.Big.bonsai
                                    |> Sprite.viewSprite attrs

                            else
                                Sprite.Big.bonsaiSapling
                                    |> Sprite.viewSprite attrs
                       )

            Just (Path dir1 dir2) ->
                Sprite.viewPath attrs dir1 dir2

            Just Sand ->
                Sprite.Big.sand
                    |> Sprite.viewSprite attrs

            Just Gras ->
                Sprite.Big.gras
                    |> Sprite.viewSprite attrs

            Just Water ->
                Sprite.Big.viewWater
                    |> Sprite.viewSprite attrs

            Just (Sign _) ->
                Sprite.Big.sign
                    |> Sprite.viewSprite attrs

            Just SolidPlaceholder ->
                [] |> Sprite.viewSprite attrs

            Nothing ->
                Sprite.Big.viewEmpty
                    |> Sprite.viewSprite attrs
