module GameSet exposing
    ( GameSet
    , Msg
    , Stats
    , getGame
    , getGames
    , hasFailed
    , hasLoading
    , hasOk
    , hasQueued
    , init
    , isEmpty
    , loadParallel
    , queue
    , queueAndWhitelist
    , requeueFailed
    , stats
    , update
    )

import Dict exposing (Dict)
import Dict.Extra
import Http
import List.Extra
import Set exposing (Set)
import Steam


type LoadState
    = Queued Int
    | Loading Int
    | Failed Int Error


type Error
    = ErrorMaxRetries
    | ErrorHttp Http.Error


type alias GameSet =
    { ok : Dict Steam.AppId Steam.GameDetails
    , missing : Set Steam.AppId
    , pending : Dict Steam.AppId LoadState

    -- settings
    , batchSize : Int
    , maxRetries : Int
    }


init : Int -> Int -> GameSet
init batchSize maxRetries =
    { ok = Dict.empty
    , missing = Set.empty
    , pending = Dict.empty

    -- settings
    , batchSize = batchSize
    , maxRetries = maxRetries
    }


isEmpty : GameSet -> Bool
isEmpty gs =
    Dict.isEmpty gs.ok
        && Set.isEmpty gs.missing
        && Dict.isEmpty gs.pending



--
-- Public manipulation API
--


{-| Queue new games
-}
queue : List Steam.AppId -> GameSet -> GameSet
queue appIds gameSet =
    case appIds of
        [] ->
            gameSet

        appId :: xs ->
            if
                Dict.member appId gameSet.ok
                    || Set.member appId gameSet.missing
                    || Dict.member appId gameSet.pending
            then
                queue xs gameSet

            else
                queue xs
                    { gameSet
                        | pending = Dict.insert appId (Queued 1) gameSet.pending
                    }


{-| Remove games not whitelisted, queue those that are new
-}
queueAndWhitelist : List Steam.AppId -> GameSet -> GameSet
queueAndWhitelist appIds gameSet =
    let
        appIdSet =
            Set.fromList appIds

        existingAppIdSet =
            Set.union
                gameSet.missing
                (Set.union
                    (Set.fromList <| Dict.keys gameSet.ok)
                    (Set.fromList <| Dict.keys gameSet.pending)
                )

        newAppIds =
            Set.diff appIdSet existingAppIdSet
                |> Set.toList
    in
    { gameSet
        | ok = Dict.Extra.keepOnly appIdSet gameSet.ok
        , missing = Set.intersect appIdSet gameSet.missing
        , pending = Dict.Extra.keepOnly appIdSet gameSet.pending
    }
        |> queue newAppIds


requeueFailed : GameSet -> GameSet
requeueFailed gameSet =
    let
        requeue : LoadState -> LoadState
        requeue state =
            case state of
                Failed x _ ->
                    Queued x

                _ ->
                    state
    in
    { gameSet
        | pending = Dict.map (always requeue) gameSet.pending
    }


requeueLoading : Bool -> List Steam.AppId -> GameSet -> GameSet
requeueLoading freezeCounter appIds gameSet =
    let
        requeue : LoadState -> LoadState
        requeue state =
            case state of
                Loading n ->
                    if freezeCounter then
                        Queued n

                    else if n > gameSet.maxRetries then
                        Failed n ErrorMaxRetries

                    else
                        Queued (n + 1)

                _ ->
                    state
    in
    case appIds of
        [] ->
            gameSet

        appId :: xs ->
            requeueLoading
                freezeCounter
                xs
                { gameSet
                    | pending = Dict.update appId (Maybe.map requeue) gameSet.pending
                }


loadParallel : (Msg -> msg) -> GameSet -> ( GameSet, Cmd msg )
loadParallel toMsg gameSet =
    let
        pullQueued :
            Steam.AppId
            -> LoadState
            -> ( List Steam.AppId, Dict Steam.AppId LoadState )
            -> ( List Steam.AppId, Dict Steam.AppId LoadState )
        pullQueued appId state ( toLoad, pending ) =
            case state of
                Queued x ->
                    ( appId :: toLoad
                    , Dict.insert appId (Loading x) pending
                    )

                _ ->
                    ( toLoad
                    , pending
                    )

        ( appIdsToLoad, queuedChangedToLoading ) =
            Dict.foldl pullQueued ( [], gameSet.pending ) gameSet.pending
    in
    case appIdsToLoad of
        [] ->
            -- Nothing queued
            ( gameSet, Cmd.none )

        _ ->
            let
                load : List Steam.AppId -> Cmd msg
                load appIds =
                    Steam.loadGames appIds (ReceiveGames appIds)
                        |> Cmd.map toMsg
            in
            ( { gameSet
                | pending = queuedChangedToLoading
              }
            , appIdsToLoad
                |> List.Extra.greedyGroupsOf gameSet.batchSize
                |> List.map load
                |> Cmd.batch
            )


type Msg
    = ReceiveGames (List Steam.AppId) (Result Http.Error (List Steam.GameResult))


update : (Msg -> msg) -> Msg -> GameSet -> ( GameSet, Cmd msg )
update toMsg msg gameSet =
    case msg of
        ReceiveGames appIds (Err err) ->
            ( failWith err appIds gameSet
            , Cmd.none
            )

        ReceiveGames appIds (Ok result) ->
            let
                importResults : List Steam.GameResult -> GameSet -> GameSet
                importResults gameResults gs =
                    case gameResults of
                        [] ->
                            gs

                        Steam.GameParseError :: xs ->
                            importResults xs gs

                        (Steam.GameFound game) :: xs ->
                            importResults xs (succeed game gs)

                        (Steam.GameRemoved appId) :: xs ->
                            importResults xs (miss appId gs)

                -- If response contains "valid" responses, it means that
                -- our batch is moving forward, and retry-counters can be
                -- reset: the backend just didn't have time to process
                -- everything yet.
                isValidResult : Steam.GameResult -> Bool
                isValidResult gameResult =
                    case gameResult of
                        Steam.GameFound _ ->
                            True

                        Steam.GameRemoved _ ->
                            True

                        _ ->
                            False
            in
            gameSet
                |> importResults result
                |> requeueLoading (List.any isValidResult result) appIds
                |> loadParallel toMsg



--
-- Public inspection API
--


type alias Stats =
    { total : Int
    , ok : Int
    , missing : Int
    , queued : Int
    , loading : Int
    , failed : Int
    }


stats : GameSet -> Stats
stats gameSet =
    let
        partialStats =
            { total =
                Dict.size gameSet.ok
                    + Set.size gameSet.missing
                    + Dict.size gameSet.pending
            , ok = Dict.size gameSet.ok
            , missing = Set.size gameSet.missing
            , queued = 0
            , loading = 0
            , failed = 0
            }

        mapper : Steam.AppId -> LoadState -> Stats -> Stats
        mapper _ state stat =
            case state of
                Queued _ ->
                    { stat | queued = stat.queued + 1 }

                Loading _ ->
                    { stat | loading = stat.loading + 1 }

                Failed _ _ ->
                    { stat | failed = stat.failed + 1 }
    in
    Dict.foldr mapper partialStats gameSet.pending


getGame : GameSet -> Steam.AppId -> Maybe Steam.GameDetails
getGame gameSet appId =
    Dict.get appId gameSet.ok


getGames : GameSet -> List Steam.GameDetails
getGames =
    .ok >> Dict.values


hasQueued : GameSet -> Bool
hasQueued =
    Dict.Extra.any (always isLoading) << .pending


hasLoading : GameSet -> Bool
hasLoading =
    Dict.Extra.any (always isLoading) << .pending


hasFailed : GameSet -> Bool
hasFailed =
    Dict.Extra.any (always isFailed) << .pending


hasOk : GameSet -> Bool
hasOk =
    not << Dict.isEmpty << .ok



-- Internal


isQueued : LoadState -> Bool
isQueued state =
    case state of
        Queued _ ->
            True

        _ ->
            False


isLoading : LoadState -> Bool
isLoading state =
    case state of
        Loading _ ->
            True

        _ ->
            False


isFailed : LoadState -> Bool
isFailed state =
    case state of
        Failed _ _ ->
            True

        _ ->
            False


succeed : Steam.GameDetails -> GameSet -> GameSet
succeed game gameSet =
    { gameSet
        | ok = Dict.insert game.appId game gameSet.ok
        , missing = Set.remove game.appId gameSet.missing
        , pending = Dict.remove game.appId gameSet.pending
    }


miss : Steam.AppId -> GameSet -> GameSet
miss appId gameSet =
    { gameSet
        | ok = Dict.remove appId gameSet.ok
        , missing = Set.insert appId gameSet.missing
        , pending = Dict.remove appId gameSet.pending
    }


failWith : Http.Error -> List Steam.AppId -> GameSet -> GameSet
failWith err appIds gameSet =
    let
        countBumper : Maybe Int -> Maybe Int
        countBumper =
            Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just
    in
    case appIds of
        [] ->
            gameSet

        appId :: xs ->
            let
                failUpdater : LoadState -> LoadState
                failUpdater state =
                    case state of
                        Queued x ->
                            Failed (x + 1) (ErrorHttp err)

                        Loading x ->
                            Failed (x + 1) (ErrorHttp err)

                        Failed x _ ->
                            Failed (x + 1) (ErrorHttp err)
            in
            failWith
                err
                xs
                { gameSet
                    | pending = Dict.update appId (Maybe.map failUpdater) gameSet.pending
                }
