module View.Overlay exposing (..)

import Config
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import View.Game exposing (viewSmallTile)


gameMenu : { startGame : msg } -> Html msg
gameMenu args =
    [ [ "🎲" |> Layout.text [ Html.Attributes.class "font-size-title" ]
      , "Game-Template" |> Layout.text [ Html.Attributes.class "font-size-big" ]
      ]
        |> Layout.column Layout.centered
    , Layout.textButton []
        { label = "Start"
        , onPress = Just args.startGame
        }
    ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--big-space)"
                :: Layout.centered
            )
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "var(--secondary-color)"
             , Html.Attributes.style "color" "white"
             ]
                ++ Layout.centered
            )


message : String -> Html msg
message string =
    [ Layout.text [] string ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--big-space)"
                :: Layout.centered
            )
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "#c5e710"
             , Html.Attributes.style "color" "black"
             ]
                ++ Layout.centered
            )


map : Game -> Html msg
map game =
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
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "white"
             , Html.Attributes.style "color" "white"
             ]
                ++ Layout.centered
            )


asFullScreenOverlay : List (Attribute msg) -> Html msg -> Html msg
asFullScreenOverlay attrs =
    Layout.el
        ([ Html.Attributes.style "position" "absolute"
         , Html.Attributes.style "inset" "0 0"
         , Html.Attributes.style "height" "100%"
         , Html.Attributes.style "width" "100%"
         ]
            ++ attrs
        )
