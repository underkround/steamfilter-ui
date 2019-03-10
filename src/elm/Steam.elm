module Steam exposing
    ( AppId
    , GameDetails
    , GameResult(..)
    , Profile
    , ProfileGame
    , SteamId64
    , loadGames
    , loadProfile
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


urls =
    --{ profile = \profileId -> "https://api.steamfilter.net/gamelist?skipCache=1&user=" ++ profileId
    { profile = \profileId -> "https://api.steamfilter.net/gamelist?user=" ++ profileId
    , gameDetails =
        \appIds ->
            "https://api.steamfilter.net/gamedetails?appId="
                ++ (List.map String.fromInt appIds |> String.join ",")
    }



-- Using string for now because of backend reasons


type alias SteamId64 =
    String


type alias AppId =
    Int


type alias Profile =
    { steamId64 : SteamId64
    , steamId : String
    , avatarIcon : String
    , games : Dict AppId ProfileGame
    }


profileApiDecoder : JD.Decoder Profile
profileApiDecoder =
    let
        profileGameListDecoder : JD.Decoder (Dict AppId ProfileGame)
        profileGameListDecoder =
            let
                gameListToDict : List ProfileGame -> Dict AppId ProfileGame
                gameListToDict =
                    List.map (\g -> ( g.appId, g )) >> Dict.fromList
            in
            JD.list profileGameDecoder
                |> JD.map gameListToDict
    in
    JD.succeed Profile
        |> JDP.required "SteamID64" JD.string
        |> JDP.required "SteamID" JD.string
        |> JDP.required "AvatarIcon" JD.string
        |> JDP.requiredAt [ "response", "games" ] profileGameListDecoder


type alias ProfileGame =
    { appId : AppId
    , playtime2weeks : Maybe Int
    , playtimeForever : Int
    }


profileGameDecoder : JD.Decoder ProfileGame
profileGameDecoder =
    JD.succeed ProfileGame
        |> JDP.required "appid" JD.int
        |> JDP.optional "playtime_2weeks" (JD.nullable JD.int) Nothing
        |> JDP.required "playtime_forever" JD.int


loadProfile : String -> (Result Http.Error Profile -> msg) -> Cmd msg
loadProfile profileId msg =
    Http.get
        { url = urls.profile profileId
        , expect = Http.expectJson msg profileApiDecoder
        }


type alias GameDetails =
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
    = GameFound GameDetails
    | GameRemoved AppId
    | GameParseError


gameDetailsDecoder : JD.Decoder GameDetails
gameDetailsDecoder =
    JD.succeed GameDetails
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
        toGame : GameDetails -> GameResult
        toGame details =
            case details.name of
                "" ->
                    GameRemoved details.appId

                _ ->
                    GameFound details
    in
    JD.list
        (JD.maybe gameDetailsDecoder
            |> JD.map (Maybe.map toGame)
            |> JD.map (Maybe.withDefault GameParseError)
        )


loadGames : List AppId -> (Result Http.Error (List GameResult) -> msg) -> Cmd msg
loadGames appIds msg =
    Http.get
        { url = urls.gameDetails appIds
        , expect = Http.expectJson msg gameDetailsApiDecoder
        }
