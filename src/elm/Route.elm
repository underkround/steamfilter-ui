module Route exposing
    ( Route(..)
    , fromUrl
    , toString
    )

import Html as H
import Html.Attributes as At
import Page.GameGrid
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type Route
    = GameGrid
    | About


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map GameGrid Parser.top
        , Parser.map About (s "about")
        ]


toString : Route -> String
toString page =
    case page of
        GameGrid ->
            ""

        About ->
            "#/about"



-- "show" :: Profile.toUrl profiles


fromUrl : Url -> Maybe Route
fromUrl url =
    --Parser.parse parser url
    -- Using fragments until we figure out if aws+s3 supports real paths
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


href : Route -> H.Attribute msg
href target =
    At.href (toString target)
