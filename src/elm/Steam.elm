module Steam exposing
    ( AppId
    , GameDetails
    , Profile
    , ProfileGameDetails
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
    { steamId64 : Int
    , details : Maybe ProfileInfo
    , games : Dict AppId ProfileGameDetails
    }


type ProfileInfo
    = ProfileInfo
        { steamId64 : Int
        , steamId : String
        , friends : Maybe (List ProfileInfo)
        }


type alias ProfileGameDetails =
    { appId : AppId
    , playtime2weeks : Maybe Int
    , playtimeForever : Int
    }


type alias GameDetails =
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



-- DECODERS


profileApiDecoder : JD.Decoder Profile
profileApiDecoder =
    JD.succeed Profile
        |> JDP.hardcoded 0
        |> JDP.hardcoded Nothing
        |> JDP.requiredAt [ "response", "games" ] profileGameDetailsDecoder


profileGameDetailsDecoder : JD.Decoder (Dict AppId ProfileGameDetails)
profileGameDetailsDecoder =
    let
        listToDict : List ProfileGameDetails -> Dict AppId ProfileGameDetails
        listToDict =
            List.map (\g -> ( g.appId, g ))
                >> Dict.fromList
    in
    JD.list
        (JD.succeed ProfileGameDetails
            |> JDP.required "appid" JD.int
            |> JDP.optional "playtime_2weeks" (JD.nullable JD.int) Nothing
            |> JDP.required "playtime_forever" JD.int
        )
        |> JD.map listToDict
