module Game exposing (..)

import Config
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Effect exposing (Effect(..))
import Point
import Random exposing (Generator)
import Structure
import Tile exposing (Obstacle(..), Tile(..))


type alias Game =
    { playerPos : ( Int, Int )
    , playerDir : Direction
    , tiles : Dict ( Int, Int ) Tile
    }


type alias Random a =
    Generator a


new : Generator Game
new =
    let
        ( x, y ) =
            ( -7, -7 )
    in
    { playerPos = ( 0, 0 )
    , playerDir = Left
    , tiles =
        [ Structure.startingArea
            |> Structure.transpose ( x, y )
        , Structure.grasArea
            |> Structure.transpose ( x - 16, y - 16 )
        , Structure.poleArea
            |> Structure.transpose ( x - 16, y + 16 )
        , Structure.grasBatches
            |> Structure.transpose ( x + 16, y - 16 )
        , Structure.shrineArea
            |> Structure.transpose ( x + 16, y + 16 )
        , Structure.stoneArea
            |> Structure.transpose ( x + 32, y - 32 )
        , Structure.smallGarden
            |> Structure.transpose ( x, y + 32 )
        ]
            |> List.concat
            |> Dict.fromList
            |> Dict.insert ( 0, -2 )
                (Sign "If you are facing an obsticale, find a way around it")
            |> Dict.insert ( 0, -5 )
                (Sign "Welcome to your own zen garden")
            |> Dict.insert ( 0, -10 )
                (Sign "Press M to open the map")
    }
        |> generateNewTiles


generateTile : List Tile -> Random Tile
generateTile neighbors =
    (if List.any Tile.isSolid neighbors then
        []

     else
        [ List.repeat 20 Sand
        , [ Bonsai ]
        ]
            |> List.concat
    )
        |> Random.uniform Sand


move : Direction -> Game -> Random ( Game, List Effect )
move dir game =
    let
        newPos =
            Direction.toPoint dir
                |> Point.add game.playerPos

        isValid =
            if Dict.get newPos game.tiles == Just Gras then
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
    case Dict.get newPos game.tiles of
        Just Gras ->
            moveOntoGras dir game

        Just Sand ->
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

        Just (Path dir1 dir2) ->
            moveOntoPath { dir = dir, path = ( dir1, dir2 ) } game

        Just (Sign string) ->
            ( game, [ ShowMessage string ] )
                |> Random.constant

        _ ->
            game
                |> Effect.withNone
                |> Random.constant


moveOverPath : { dir : Direction, path : ( Direction, Direction ) } -> Game -> Random ( Game, List Effect )
moveOverPath args game =
    let
        ( dir1, dir2 ) =
            args.path

        newPos =
            Direction.toPoint args.dir
                |> Point.add game.playerPos
    in
    if args.dir == dir2 || args.dir == dir1 then
        { game
            | tiles =
                case Dict.get newPos game.tiles of
                    Just Sand ->
                        game.tiles

                    _ ->
                        game.tiles |> Dict.insert game.playerPos Sand
            , playerDir = args.dir
            , playerPos = newPos
        }
            |> Effect.withNone
            |> Random.constant

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


generateNewTiles : Game -> Random Game
generateNewTiles game =
    let
        ( playerX, playerY ) =
            game.playerPos
    in
    List.range (playerX - Config.cameraRadius) (playerX + Config.cameraRadius)
        |> List.concatMap
            (\x ->
                List.range (playerY - Config.cameraRadius) (playerY + Config.cameraRadius)
                    |> List.map (Tuple.pair x)
            )
        |> List.filter (\p -> Dict.get p game.tiles == Nothing)
        |> List.foldl
            (\pos ->
                Random.andThen
                    (\g ->
                        neighborsOf pos game
                            |> generateTile
                            |> Random.map
                                (\tile ->
                                    insert pos tile g
                                )
                    )
            )
            (Random.constant game)


insert : ( Int, Int ) -> Tile -> Game -> Game
insert pos tile game =
    { game | tiles = Dict.insert pos tile game.tiles }


neighborsOf : ( Int, Int ) -> Game -> List Tile
neighborsOf ( x, y ) game =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.filterMap (\p -> Dict.get p game.tiles)
