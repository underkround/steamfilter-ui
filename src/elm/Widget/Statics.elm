module Widget.Statics exposing
    ( footer
    , header
    )

import Html as H exposing (Html)
import Html.Attributes as At


header : Html msg
header =
    H.header
        []
        [ H.h1 [] [ H.text "Steam Filter" ]
        ]


footer : Html msg
footer =
    H.footer
        []
        [ H.text "Footer" ]
