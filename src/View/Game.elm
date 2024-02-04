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

        plantsMet =
            game.tiles
                |> Dict.toList
                |> List.filterMap
                    (\( pos, tile ) ->
                        if tile == Bonsai then
                            game
                                |> Game.neighborsOf pos
                                |> (\neighbors ->
                                        if
                                            List.any
                                                (\t ->
                                                    case t of
                                                        Path _ _ ->
                                                            True

                                                        _ ->
                                                            False
                                                )
                                                neighbors
                                        then
                                            Just neighbors

                                        else
                                            Nothing
                                   )

                        else
                            Nothing
                    )
    in
    [ List.range (playerY - Config.cameraRadius)
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
        |> Html.div
            [ Html.Style.positionRelative
            , Html.Style.height (String.fromInt ((1 + Config.cameraRadius * 2) * Config.tileSize) ++ "px")
            ]
    , "Stars:"
        ++ String.fromInt game.starsCollected
        ++ "/"
        ++ String.fromInt Config.starsAmount
        |> Layout.text []
    , plantsMet
        |> List.filter
            (\neighbors ->
                neighbors
                    |> List.member Sand
            )
        |> List.length
        |> (\n ->
                [ "Sad Plants:" ++ String.fromInt n |> Layout.text []
                , "Happy Plants:" ++ String.fromInt (List.length plantsMet - n) |> Layout.text []
                ]
                    |> Layout.column []
           )
    ]
        |> Layout.column []


viewSmallTile : ( Int, Int ) -> Game -> Html msg
viewSmallTile ( x, y ) game =
    Dict.get ( x, y ) game.tiles
        |> Maybe.map
            (\tile ->
                case tile of
                    ObstacleTile _ ->
                        [ "⬛⬛"
                        , "⬛⬛"
                        ]

                    Sign _ ->
                        [ "⬛⬛"
                        , "⬛⬛"
                        ]

                    Bonsai ->
                        if
                            Game.neighborsOf ( x, y ) game
                                |> List.all ((/=) Sand)
                        then
                            [ "⬜🟩"
                            , "⬜⬜"
                            ]

                        else
                            [ "⬜⬛"
                            , "⬜⬜"
                            ]

                    Path dir1 dir2 ->
                        Sprite.Small.path dir1 dir2

                    Sand ->
                        [ "⬜⬜"
                        , "⬜⬜"
                        ]

                    Gras ->
                        [ "🟩🟩"
                        , "🟩🟩"
                        ]

                    Water ->
                        [ "🟦🟦"
                        , "🟦🟦"
                        ]

                    Statue ->
                        [ "⬛⬛"
                        , "⬛⬛"
                        ]

                    Shrine ->
                        [ "⬛⬛"
                        , "⬛⬛"
                        ]

                    Star ->
                        [ "🟨🟨"
                        , "🟨🟨"
                        ]

                    SolidPlaceholder ->
                        [ "⬛⬛"
                        , "⬛⬛"
                        ]
            )
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
                if
                    Game.neighborsOf ( x, y ) game
                        |> List.all ((/=) Sand)
                then
                    Sprite.Big.bonsai
                        |> Sprite.viewSprite attrs

                else
                    Sprite.Big.bonsaiSapling
                        |> Sprite.viewSprite attrs

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

            Just Star ->
                Sprite.Big.star
                    |> Sprite.viewSprite attrs

            Just SolidPlaceholder ->
                [] |> Sprite.viewSprite attrs

            Nothing ->
                Sprite.Big.viewEmpty
                    |> Sprite.viewSprite attrs
