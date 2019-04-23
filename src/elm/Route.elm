module Route exposing
    ( Route(..)
    , fromUrl
    , href
    )

import GameGrid
import Html as H
import Html.Attributes as At
import Steam.Game
import Steam.Profile
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type Route
    = Top
    | Filter (List String)
    | About


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Filter (s "filter" </> Steam.Profile.urlParser)
        , Parser.map About (s "about")
        ]


href : Route -> H.Attribute msg
href target =
    At.href (toUrlString target)


fromUrl : Url -> Maybe Route
fromUrl url =
    --Parser.parse parser url
    -- Using fragments until we figure out if aws+s3 supports real paths
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


toUrlString : Route -> String
toUrlString page =
    let
        parts =
            case page of
                Top ->
                    []

                Filter profiles ->
                    [ "filter" ++ Steam.Profile.toUrlString profiles ]

                About ->
                    [ "about" ]
    in
    "#/" ++ String.join "/" parts
