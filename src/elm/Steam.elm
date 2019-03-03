module Steam exposing
    ( AppId
    , Game
    , Profile
    , ProfileGame
    , loadProfile
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Lib.Remote as Remote exposing (Remote)


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
    , logo : String
    , storeLink : String
    , globalStatsLink : String
    }



-- PUBLIC API


loadProfile : String -> (Result Http.Error Profile -> msg) -> Cmd msg
loadProfile profileId msg =
    let
        -- @TODO: build url properly
        url : String
        url =
            "https://api.steamfilter.net/gamelist?user=" ++ profileId
    in
    Http.get
        { url = url
        , expect = Http.expectJson msg profileApiDecoder
        }
