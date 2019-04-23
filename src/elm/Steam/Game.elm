module Steam.Game exposing
    ( AppId
    , Game
    , GameResult(..)
    , load
    )

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


loadUrl : List AppId -> String
loadUrl appIds =
    "https://api.steamfilter.net/gamedetails?appId="
        ++ (List.map String.fromInt appIds |> String.join ",")


type alias AppId =
    Int


type alias Game =
    { appId : AppId
    , name : String
    , icon : String
    , developer : String
    , publisher : String

    --, storeLink : String
    , features : List String
    , genres : List String
    , releaseDate : Int
    }


type GameResult
    = GameFound Game
    | GameRemoved AppId
    | GameParseError


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.succeed Game
        |> JDP.required "AppId" JD.int
        |> JDP.required "Name" JD.string
        |> JDP.required "Icon" JD.string
        |> JDP.optional "Developer" JD.string ""
        |> JDP.optional "Publisher" JD.string ""
        |> JDP.optional "Features" (JD.list JD.string) []
        |> JDP.optional "Genres" (JD.list JD.string) []
        |> JDP.required "ReleaseDate" JD.int


gameDetailsApiDecoder : JD.Decoder (List GameResult)
gameDetailsApiDecoder =
    let
        toResult : Game -> GameResult
        toResult game =
            case game.name of
                "" ->
                    GameRemoved game.appId

                _ ->
                    GameFound game
    in
    JD.list
        (JD.maybe gameDecoder
            |> JD.map (Maybe.map toResult)
            |> JD.map (Maybe.withDefault GameParseError)
        )


load : List AppId -> (Result Http.Error (List GameResult) -> msg) -> Cmd msg
load appIds msg =
    Http.get
        { url = loadUrl appIds
        , expect = Http.expectJson msg gameDetailsApiDecoder
        }
