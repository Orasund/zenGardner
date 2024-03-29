module View exposing (..)

import Config
import Html exposing (Html)
import Html.Viewport


viewportMeta : Html msg
viewportMeta =
    Html.Viewport.fixedWidth []
        --device-width
        Config.screenMinWidth


stylesheet : Html msg
stylesheet =
    --In-Elm Stylesheet is usually easier to load by itch.io
    """@font-face {
    font-family: "NotoEmoji";
    src: url("fonts/NotoEmoji-Regular.ttf");
}

@font-face {
    font-family: "NotoColorEmoji";
    src: url("fonts/NotoColorEmoji-Regular.ttf");
}

:root {
    --primary-color: #ffc608;
    --secondary-color: #6f7ce3;

    --background-color: white;


    --small-space: calc(var(--space) / 2);
    --space: 16px;
    --big-space: calc(var(--space) * 2);
    --normal-font-size: 16px;
    --small-font-size: calc(var(--normal-font-size) * 0.5);
    --big-font-size: calc(var(--normal-font-size) * 2);
    --title-font-size: calc(var(--normal-font-size) * 4);
}

html {
    margin: 0;
    padding: 0;

    font-family: sans-serif, "NotoColorEmoji";
    color: var(--font-color);
}

body {
    width: 100%;
    height: 100%;
    margin: 0;
    padding: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    background-color: white;
}

.container {
    position: relative;
    background-color: var(--background-color);
    border-radius: var(--space);
    box-sizing: border-box;
    overflow: hidden;
}

.content {
    padding: var(--space);
}

button {
    border: 0;
    background-color: var(--primary-color);
    border-radius: var(--small-space);
    padding: var(--small-space) var(--space);
}

button:hover,
button:focus {
    filter: brightness(0.9);
}

button:active {
    filter: brightness(0.75);
}

/***************************************************
 * UTILITY CLASSES
 ***************************************************/

.font-size-normal {
    font-size: var(--normal-font-size);
}

.font-size-small {
    font-size: var(--small-font-size);
}

.font-size-big {
    font-size: var(--big-font-size);
}

.font-size-title {
    font-size: var(--title-font-size);
}

.emoji-font {
    font-family: "NotoColorEmoji";
}

.emoji-color-font {
    font-family: "NotoColorEmoji";
}""" |> Html.text |> List.singleton |> Html.node "style" []
