module Sprite.Big exposing (..)


viewWater : List String
viewWater =
    [ "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    , "🟦🟦🟦🟦🟦🟦🟦🟦"
    ]


viewPlayer : List String
viewPlayer =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬛⬜⬜⬛⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


viewEmpty : List String
viewEmpty =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


stone : List String
stone =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬛⬜"
    , "⬜⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬜⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


pole : List String
pole =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬛⬜⬜⬛⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬛⬜⬜⬛⬜⬜"
    , "⬜⬜⬛⬜⬜⬛⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


curvedPath : List String
curvedPath =
    [ "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜🟨🟨⬜⬜⬜⬜"
    , "🟨🟨🟨🟨⬜⬜⬜⬜"
    , "🟨🟨⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


straightPath : List String
straightPath =
    [ "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    , "⬜⬜⬜🟨🟨⬜⬜⬜"
    ]


sand : List String
sand =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜🟨⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜🟨⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜🟨⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜🟨⬜⬜⬜⬜⬜⬜"
    ]


gras : List String
gras =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜🟩⬜⬜⬜⬜⬜"
    , "⬜⬜⬜🟩⬜⬜🟩⬜"
    , "⬜🟩⬜🟩⬜🟩⬜⬜"
    , "⬜🟩⬜🟩⬜🟩⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


sign : List String
sign =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬛⬜⬜⬜⬜⬛⬜"
    , "⬜⬛⬜⬜⬜⬜⬛⬜"
    , "⬜⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


bonsai : List String
bonsai =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜🟩🟩⬜⬜⬜"
    , "⬜⬜🟩🟩🟩🟩⬜⬜"
    , "⬜⬜⬜⬛⬛⬜🟩⬜"
    , "⬜🟩🟩⬜⬛🟩🟩🟩"
    , "⬜⬜⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬛⬛⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


bonsaiSapling : List String
bonsaiSapling =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬛⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬛⬜⬜⬜"
    , "⬜⬜⬜⬜⬛⬛⬜⬜"
    , "⬜⬜⬜⬜⬛⬛⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


statue : List String
statue =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬛⬛⬛⬛⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬛⬛⬛⬛⬛⬛⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬛⬛⬜⬜⬛⬛⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬛⬜⬜⬛⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬛⬜⬛⬛⬜⬛⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬛⬜⬜⬜⬜⬜⬜⬛⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬛⬜⬜⬜⬜⬜⬜⬛⬜⬜⬜⬜"
    , "⬜⬜⬜⬛⬜⬜⬜⬜⬜⬜⬜⬜⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬜⬜⬛⬛⬛⬛⬜⬜⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬜⬜⬜⬜⬜⬜⬜⬜⬛⬜⬜⬜"
    , "⬜⬜⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬜⬛⬜⬜"
    , "⬜⬜⬛⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬛⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    ]


shrine : List String
shrine =
    [ "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛"
    , "⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬛⬛⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬛⬛⬜⬜⬜⬜⬜⬜⬛⬛⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    , "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
    ]
