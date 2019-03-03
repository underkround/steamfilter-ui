module Steam exposing
    ( AppId
    , Game
    , Profile
    , ProfileGame
    , loadGames
    , loadProfile
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Lib.Remote as Remote exposing (Remote)


urls =
    --{ profile = \profileId -> "https://api.steamfilter.net/gamelist?skipCache=1&user=" ++ profileId
    { profile = \profileId -> "https://api.steamfilter.net/gamelist?user=" ++ profileId
    , gameDetails =
        \appIds ->
            "https://api.steamfilter.net/gamedetails?appId="
                ++ (List.map String.fromInt appIds |> String.join ",")
    }


type alias AppId =
    Int


type alias Profile =
    { steamId64 : String
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


type alias Game =
    { appId : AppId
    , name : String
    , icon : String

    --, storeLink : String
    , features : List String

    --, genres : List String
    --, releaseDate : String
    }


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.succeed Game
        |> JDP.required "AppId" JD.int
        |> JDP.required "Name" JD.string
        |> JDP.required "Icon" JD.string
        |> JDP.optional "Features" (JD.list JD.string) []


gameDetailsApiDecoder : JD.Decoder (List Game)
gameDetailsApiDecoder =
    JD.list gameDecoder



-- PUBLIC API


loadProfile : String -> (Result Http.Error Profile -> msg) -> Cmd msg
loadProfile profileId msg =
    Http.get
        { url = urls.profile profileId
        , expect = Http.expectJson msg profileApiDecoder
        }


loadGames : List AppId -> (Result Http.Error (List Game) -> msg) -> Cmd msg
loadGames appIds msg =
    Http.get
        { url = urls.gameDetails appIds
        , expect = Http.expectJson msg gameDetailsApiDecoder
        }
