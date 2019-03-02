module Steam.Profile exposing
    ( Model
    , Profile
    , urlParser
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type alias Model =
    List Profile


type alias Profile =
    String


parser : Parser (Route -> a) a
parser =
    Parser.string
