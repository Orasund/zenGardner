module View.Overlay exposing (..)

import Config
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import View.Game exposing (viewSmallTile)


gameWon : Html msg
gameWon =
    [ Layout.text [ Html.Attributes.style "font-size" "16px" ]
        "You found all Stars"
    , Layout.text [ Html.Attributes.style "font-size" "24px" ]
        "Thanks for playing"
    ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--big-space)"
                :: Layout.centered
            )
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "white"
             , Html.Attributes.style "color" "black"
             , Html.Attributes.style "font-size" "24px"
             ]
                ++ Layout.centered
            )


message : String -> Html msg
message string =
    [ Layout.text
        [ Html.Attributes.style "padding" "16px"
        ]
        string
    ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--big-space)"
                :: Layout.centered
            )
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "white"
             , Html.Attributes.style "color" "black"
             , Html.Attributes.style "font-size" "24px"
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
