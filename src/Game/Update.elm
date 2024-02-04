module Game.Update exposing (..)

import Dict
import Direction exposing (Direction)
import Effect exposing (Effect(..))
import Game exposing (Game, generateNewTiles)
import Point
import Random exposing (Generator)
import Tile exposing (Tile(..))


type alias Random a =
    Generator a


move : Direction -> Game -> Random ( Game, List Effect )
move dir game =
    let
        newPos =
            Direction.toPoint dir
                |> Point.add game.playerPos
    in
    case Dict.get newPos game.tiles of
        Just Star ->
            { game
                | starsCollected = game.starsCollected + 1
                , tiles =
                    game.tiles
                        |> Dict.insert newPos Sand
            }
                |> moveOntoSand dir

        Just Gras ->
            moveOntoGras dir game

        Just Sand ->
            moveOntoSand dir game

        Just (Path dir1 dir2) ->
            moveOntoPath { dir = dir, path = ( dir1, dir2 ) } game

        Just (Sign string) ->
            ( game, [ ShowMessage string ] )
                |> Random.constant

        _ ->
            game
                |> Effect.withNone
                |> Random.constant


moveOntoSand : Direction -> Game -> Random ( Game, List Effect )
moveOntoSand dir game =
    let
        newPos =
            Direction.toPoint dir
                |> Point.add game.playerPos

        isValid =
            if Dict.get newPos game.tiles /= Just Sand then
                True

            else if Dict.get game.playerPos game.tiles /= Just Sand then
                True

            else if game.playerDir == dir then
                [ Direction.rotClockwise dir
                    |> Direction.toPoint
                    |> Point.add game.playerPos
                , Direction.rotCounterclock dir
                    |> Direction.toPoint
                    |> Point.add game.playerPos
                , Direction.rotClockwise dir
                    |> Direction.toPoint
                    |> Point.add newPos
                , Direction.rotCounterclock dir
                    |> Direction.toPoint
                    |> Point.add newPos
                ]
                    |> List.any
                        (\p ->
                            Dict.get p game.tiles
                                |> Maybe.map Tile.isSolid
                                |> Maybe.withDefault True
                        )

            else
                [ Direction.rotClockwise dir
                    |> Direction.toPoint
                    |> Point.add newPos
                    |> (\p -> Dict.get p game.tiles)
                    |> Maybe.map Tile.isSolid
                    |> Maybe.withDefault True
                , Direction.rotCounterclock dir
                    |> Direction.toPoint
                    |> Point.add newPos
                    |> (\p -> Dict.get p game.tiles)
                    |> Maybe.map Tile.isSolid
                    |> Maybe.withDefault True
                , game.playerDir
                    |> Direction.toPoint
                    |> Point.add game.playerPos
                    |> (\p -> Dict.get p game.tiles)
                    |> Maybe.map Tile.isSolid
                    |> Maybe.withDefault True
                ]
                    |> List.any identity
    in
    if isValid then
        { game
            | tiles =
                if Dict.get game.playerPos game.tiles == Just Gras then
                    game.tiles

                else
                    game.tiles
                        |> Dict.insert game.playerPos
                            (Path
                                (game.playerDir
                                    |> Direction.rotClockwise
                                    |> Direction.rotClockwise
                                )
                                dir
                            )
            , playerPos = newPos
            , playerDir = dir
        }
            |> generateNewTiles
            |> Random.map Effect.withNone

    else
        game
            |> Effect.withNone
            |> Random.constant


moveOntoPath : { dir : Direction, path : ( Direction, Direction ) } -> Game -> Random ( Game, List Effect )
moveOntoPath args game =
    let
        ( dir1, dir2 ) =
            args.path

        newPos =
            Direction.toPoint args.dir
                |> Point.add game.playerPos

        maybeNewDir =
            if
                (dir1
                    |> Direction.rotClockwise
                    |> Direction.rotClockwise
                )
                    |> (==) args.dir
            then
                Just dir2

            else if
                dir2
                    |> Direction.rotClockwise
                    |> Direction.rotClockwise
                    |> (==) args.dir
            then
                Just dir1

            else
                Nothing
    in
    maybeNewDir
        |> Maybe.map
            (\newDir ->
                { game
                    | tiles =
                        game.tiles |> Dict.insert newPos Sand
                    , playerPos = newPos
                    , playerDir =
                        newDir
                            |> Direction.rotClockwise
                            |> Direction.rotClockwise
                }
            )
        |> Maybe.withDefault game
        |> Effect.withNone
        |> Random.constant


moveOntoGras : Direction -> Game -> Random ( Game, List Effect )
moveOntoGras dir game =
    let
        newPos =
            Direction.toPoint dir
                |> Point.add game.playerPos
    in
    { game
        | tiles =
            if
                Dict.get game.playerPos game.tiles
                    == Just Gras
                    || game.playerDir
                    == (dir
                            |> Direction.rotClockwise
                            |> Direction.rotClockwise
                       )
            then
                game.tiles

            else
                game.tiles
                    |> Dict.insert game.playerPos
                        (Path
                            (game.playerDir
                                |> Direction.rotClockwise
                                |> Direction.rotClockwise
                            )
                            dir
                        )
        , playerPos = newPos
        , playerDir = dir
    }
        |> generateNewTiles
        |> Random.map Effect.withNone
