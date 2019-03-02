module Steam.Api exposing
    ( ApiError
    , ApiResult
    , loadProfileXml
    )

import Dict exposing (Dict)
import Http
import Steam.Types as ST
import Xml.Decode as XD


type ApiError
    = HttpError Http.Error
    | DecodeError String


type ApiResult
    = ApiXmlResult ST.Profile (List ST.GameDetails)



--loadProfileXml : String -> (Result ApiError ApiResult -> msg) -> Cmd msg


loadProfileXml : String -> (Result ApiError ST.Profile -> msg) -> Cmd msg
loadProfileXml profileId handler =
    let
        -- @TODO: build url properly
        url =
            "https://api.steamfilter.net/gamelist?user=" ++ profileId

        parseResult : Result Http.Error String -> msg
        parseResult result =
            case result of
                Result.Err httpError ->
                    Result.Err (HttpError httpError)
                        |> handler

                Result.Ok xmlString ->
                    XD.decodeString profileXmlDecoder (fixXmlString xmlString)
                        |> Result.mapError DecodeError
                        |> handler

        -- Hacky fix for xml lib that can't decode <![CNAME[foobar[0]]]>
        fixXmlString : String -> String
        fixXmlString =
            String.replace "]]]" "] ]]"
    in
    Http.get
        { url = url
        , expect = Http.expectString parseResult
        }


profileXmlDecoder : XD.Decoder ST.Profile
profileXmlDecoder =
    let
        profile : Int -> String -> ST.Profile
        profile steamId64 steamId =
            { steamId64 = steamId64
            , steamId = steamId
            , games = Dict.empty
            }
    in
    XD.map2 profile
        (XD.path [ "steamID64" ] (XD.single XD.int))
        (XD.path [ "steamID" ] (XD.single XD.string))
