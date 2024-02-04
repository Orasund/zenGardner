module Game exposing (Game, generateNewTiles, neighborsOf, new)

import Config
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Effect exposing (Effect(..))
import Random exposing (Generator)
import Structure
import Tile exposing (Obstacle(..), Tile(..))


type alias Game =
    { playerPos : ( Int, Int )
    , playerDir : Direction
    , tiles : Dict ( Int, Int ) Tile
    , starsCollected : Int
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
    , starsCollected = 0
    , tiles =
        [ Structure.startingArea
            |> Structure.transpose ( x, y )
        , Structure.grasCircle1
            |> Structure.transpose ( x - 16, y - 16 )
        , Structure.grasCircle2
            |> Structure.transpose ( x - 16, y + 16 )
        , Structure.grasCircle3
            |> Structure.transpose ( x + 16, y - 16 )
        , Structure.grasCircle4
            |> Structure.transpose ( x + 16, y + 16 )
        , Structure.grasBatches1
            |> Structure.transpose ( x, y + 32 )
        , Structure.grasBatches2
            |> Structure.transpose ( x, y - 32 )
        , Structure.grasBatches3
            |> Structure.transpose ( x - 32, y )
        , Structure.grasBatches4
            |> Structure.transpose ( x + 32, y )
        , Structure.poleArea
            |> Structure.transpose ( x - 32, y + 32 )
        , Structure.smallGarden
            |> Structure.transpose ( x + 32, y - 32 )
        , Structure.stoneArea
            |> Structure.transpose ( x + 32, y + 32 )
        , Structure.shrineArea
            |> Structure.transpose ( x - 32, y - 32 )
        ]
            |> List.concat
            |> Dict.fromList
            |> Dict.insert ( 0, -2 )
                (Sign "If you are facing an obsticale, find a way around it")
            |> Dict.insert ( -3, -4 )
                (Sign "Welcome to your own zen garden")
            |> Dict.insert ( -4, -9 )
                (Sign "Press M to open the map")
            |> Dict.insert ( -6, -12 )
                (Sign "Make plants happy, by incircling them")
            |> Dict.insert ( -7, -14 )
                ("Collect all "
                    ++ String.fromInt Config.starsAmount
                    ++ " Stars to win the game"
                    |> Sign
                )
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
