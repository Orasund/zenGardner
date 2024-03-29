module Main exposing (..)

import Browser
import Browser.Events
import Config
import Direction exposing (Direction(..))
import Effect exposing (Effect(..))
import Game exposing (Game)
import Game.Update
import Gen.Sound exposing (Sound(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Layout
import Overlay exposing (Overlay(..))
import Random exposing (Generator, Seed)
import View
import View.Game
import View.Overlay


type alias Model =
    { game : Game
    , overlay : Maybe Overlay
    , seed : Seed
    }


type Msg
    = NewGame
    | SetOverlay (Maybe Overlay)
    | GotSeed Seed
    | Move Direction
    | Idle
    | OpenMap


apply : Model -> Generator Model -> Model
apply { seed } generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( game, seed ) =
            Random.step Game.new (Random.initialSeed 42)
    in
    ( { game = game
      , seed = seed
      , overlay = Nothing
      }
    , Random.generate GotSeed Random.independentSeed
    )


newGame : Model -> Model
newGame model =
    Game.new
        |> Random.map (\game -> { model | game = game })
        |> apply model
        |> setOverlay Nothing


gotSeed : Seed -> Model -> Model
gotSeed seed model =
    { model | seed = seed }


setOverlay : Maybe Overlay -> Model -> Model
setOverlay maybeOverlay model =
    { model | overlay = maybeOverlay }


applyEffect : Effect -> Model -> Model
applyEffect effect model =
    case effect of
        ShowMessage string ->
            { model | overlay = Just (Message string) }

        GameIsWon ->
            { model | overlay = Just GameWon }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withNoCmd a =
            ( a, Cmd.none )
    in
    case model.overlay of
        Just _ ->
            { model | overlay = Nothing } |> withNoCmd

        Nothing ->
            case msg of
                NewGame ->
                    newGame model |> withNoCmd

                GotSeed seed ->
                    model |> gotSeed seed |> withNoCmd

                SetOverlay maybeOverlay ->
                    model |> setOverlay maybeOverlay |> withNoCmd

                Move dir ->
                    Game.Update.move dir model.game
                        |> Random.map
                            (\( game, effects ) ->
                                effects
                                    |> List.foldl applyEffect
                                        { model | game = game }
                            )
                        |> apply model
                        |> withNoCmd

                OpenMap ->
                    { model
                        | overlay =
                            if model.overlay == Just Map then
                                Nothing

                            else
                                Just Map
                    }
                        |> withNoCmd

                Idle ->
                    model |> withNoCmd


viewOverlay : Model -> Overlay -> Html Msg
viewOverlay model overlay =
    case overlay of
        GameWon ->
            View.Overlay.gameWon

        Map ->
            View.Overlay.map model.game

        Message string ->
            View.Overlay.message string


view :
    Model
    ->
        { title : String
        , body : List (Html Msg)
        }
view model =
    let
        content =
            View.Game.toHtml model.game
    in
    { title = Config.title
    , body =
        [ View.viewportMeta
        , View.stylesheet
        , model.overlay
            |> Maybe.map (viewOverlay model)
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
            |> (::) (content |> Layout.el [ Html.Attributes.class "content" ])
            |> Html.div
                [ Html.Attributes.style "width" (String.fromFloat Config.screenMinWidth ++ "px")
                , Html.Attributes.style "height" (String.fromFloat Config.screenMinHeight ++ "px")
                , Html.Attributes.class "container"
                ]
        ]
    }


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toDirection (Json.Decode.field "key" Json.Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "a" ->
            Move Left

        "d" ->
            Move Right

        "w" ->
            Move Up

        "s" ->
            Move Down

        "m" ->
            OpenMap

        _ ->
            Idle


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
