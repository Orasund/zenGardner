module View.Sprite exposing (..)

import Direction exposing (Direction(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Image
import Sprite.Big


fromMatrix :
    List (Attribute msg)
    ->
        { columns : Int
        , rows : Int
        , pixelSize : Float
        }
    -> List String
    -> Html msg
fromMatrix attrs args rows =
    rows
        |> matrixToDict
        |> Image.bitmap attrs args


matrixToDict : List String -> List ( ( Int, Int ), String )
matrixToDict rows =
    rows
        |> List.indexedMap
            (\y string ->
                string
                    |> String.toList
                    |> List.indexedMap
                        (\x a ->
                            ( ( x, y )
                            , case a of
                                '⬜' ->
                                    "white"

                                '🟨' ->
                                    "#c5e710"

                                '🟩' ->
                                    "green"

                                '🟦' ->
                                    "blue"

                                '⬛' ->
                                    "black"

                                _ ->
                                    "white"
                            )
                        )
            )
        |> List.concat


viewSprite : List (Attribute msg) -> List String -> Html msg
viewSprite attrs =
    fromMatrix attrs
        { columns = 8
        , rows = 8
        , pixelSize = 4
        }


viewLargeSprite : List (Attribute msg) -> List String -> Html msg
viewLargeSprite attrs =
    fromMatrix attrs
        { columns = 16
        , rows = 16
        , pixelSize = 4
        }


viewSmallSprite : List (Attribute msg) -> List String -> Html msg
viewSmallSprite attrs =
    fromMatrix attrs
        { columns = 2
        , rows = 2
        , pixelSize = 4
        }


viewPath : List (Attribute msg) -> Direction -> Direction -> Html msg
viewPath attrs dir1 dir2 =
    case ( dir1, dir2 ) of
        ( Up, Down ) ->
            Sprite.Big.straightPath
                |> viewSprite attrs

        ( Up, Up ) ->
            Sprite.Big.straightPath
                |> viewSprite attrs

        ( Down, Down ) ->
            Sprite.Big.straightPath
                |> viewSprite attrs

        ( Down, Up ) ->
            Sprite.Big.straightPath
                |> viewSprite attrs

        ( Left, Left ) ->
            Sprite.Big.straightPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Right, Right ) ->
            Sprite.Big.straightPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Left, Right ) ->
            Sprite.Big.straightPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Right, Left ) ->
            Sprite.Big.straightPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Left, Up ) ->
            Sprite.Big.curvedPath
                |> viewSprite attrs

        ( Up, Left ) ->
            Sprite.Big.curvedPath
                |> viewSprite attrs

        ( Up, Right ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Right, Up ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(90deg)" :: attrs)

        ( Right, Down ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(180deg)" :: attrs)

        ( Down, Right ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(180deg)" :: attrs)

        ( Down, Left ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(-90deg)" :: attrs)

        ( Left, Down ) ->
            Sprite.Big.curvedPath
                |> viewSprite (Html.Attributes.style "transform" "rotate(-90deg)" :: attrs)
