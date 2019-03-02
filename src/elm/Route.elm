module Route exposing
    ( Route(..)
    , fromUrl
    , toString
    )

import Html as H
import Html.Attributes as At
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)



--import Steam.Profile as Profile exposing (Profile)


type Route
    = Home
    | GameGrid



-- ShowGameGrid (List Profile)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map GameGrid (s "show")

        --, Parser.map GameGrid (s "show" </> Profile.urlParser)
        ]


toString : Route -> String
toString page =
    case page of
        Home ->
            "#/"

        GameGrid ->
            "#/show"



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
