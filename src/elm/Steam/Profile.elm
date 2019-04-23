module Steam.Profile exposing
    ( GameStat
    , Profile
    , SteamId64
    , load
    , toUrlString
    , urlParser
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Steam.Game
import Url.Parser


loadUrl : String -> String
loadUrl mixedProfileId =
    "https://api.steamfilter.net/gamelist?user=" ++ mixedProfileId


{-| Using string for now because of backend reasons
-}
type alias SteamId64 =
    String


type alias Profile =
    { steamId64 : SteamId64
    , steamId : String
    , avatarIcon : String
    , games : Dict Steam.Game.AppId GameStat
    }


type alias GameStat =
    { appId : Steam.Game.AppId
    , playtime2weeks : Maybe Int
    , playtimeForever : Int
    }


toUrlString : List String -> String
toUrlString profiles =
    String.join "," profiles


urlParser : Url.Parser.Parser (List String -> a) a
urlParser =
    let
        --stringList : String -> Url.Parser.Parser (List String -> a) a
        stringList splitter =
            Url.Parser.custom "STRING_LIST" <|
                \segment ->
                    Just (String.split splitter segment)
    in
    stringList ","


load : String -> (Result Http.Error Profile -> msg) -> Cmd msg
load mixedProfileId msg =
    Http.get
        { url = loadUrl mixedProfileId
        , expect = Http.expectJson msg apiDecoder
        }


apiDecoder : JD.Decoder Profile
apiDecoder =
    let
        gameStatListDecoder : JD.Decoder (Dict Steam.Game.AppId GameStat)
        gameStatListDecoder =
            let
                gameListToDict : List GameStat -> Dict Steam.Game.AppId GameStat
                gameListToDict =
                    List.map (\g -> ( g.appId, g )) >> Dict.fromList
            in
            JD.list gameStatDecoder
                |> JD.map gameListToDict

        gameStatDecoder : JD.Decoder GameStat
        gameStatDecoder =
            JD.succeed GameStat
                |> JDP.required "appid" JD.int
                |> JDP.optional "playtime_2weeks" (JD.nullable JD.int) Nothing
                |> JDP.required "playtime_forever" JD.int
    in
    JD.succeed Profile
        |> JDP.required "SteamID64" JD.string
        |> JDP.required "SteamID" JD.string
        |> JDP.required "AvatarIcon" JD.string
        |> JDP.requiredAt [ "response", "games" ] gameStatListDecoder
