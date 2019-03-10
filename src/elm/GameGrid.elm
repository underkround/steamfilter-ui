module GameGrid exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Dict exposing (Dict)
import Filters exposing (Filters)
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Http
import Lib.RemoteSet as RemoteSet exposing (RemoteSet)
import Lib.Util as Util
import Set exposing (Set)
import Steam


type Status
    = HttpError Http.Error
    | ProfilePending
    | DuplicateProfileError
    | Info String


type alias Model =
    { profiles : Dict Steam.SteamId64 Steam.Profile
    , gameDetails : Dict Steam.AppId Steam.GameDetails
    , gameProgress : RemoteSet Http.Error Steam.AppId
    , matching : List Steam.GameDetails
    , query : String
    , status : Maybe Status
    , filters : Filters
    }


batchSize : Int
batchSize =
    30


init : Model
init =
    { profiles = Dict.empty
    , gameDetails = Dict.empty
    , gameProgress = RemoteSet.init 2
    , matching = []
    , query = ""
    , status = Nothing
    , filters = Filters.init
    }


type Msg
    = LoadProfile
    | ReceiveProfile (Result Http.Error Steam.Profile)
    | ReceiveGames (List Steam.AppId) (Result Http.Error (List Steam.GameResult))
    | OnInput String
    | OnKeyCodeDown Int
    | RemoveProfile String
    | FiltersMsg Filters.Msg


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        OnInput query ->
            ( { model | query = query }
            , Cmd.none
            )

        OnKeyCodeDown keyCode ->
            case keyCode of
                13 ->
                    update toMsg LoadProfile model

                _ ->
                    ( model, Cmd.none )

        --
        -- PROFILES
        --
        LoadProfile ->
            ( { model | status = Just ProfilePending }
            , Steam.loadProfile model.query ReceiveProfile
                |> Cmd.map toMsg
            )

        ReceiveProfile (Ok profile) ->
            let
                ( status, profiles ) =
                    if Dict.member profile.steamId64 model.profiles then
                        ( Just DuplicateProfileError
                        , model.profiles
                        )

                    else
                        ( Nothing
                        , Dict.insert profile.steamId64 profile model.profiles
                        )
            in
            ( { model
                | profiles = profiles
                , status = status
                , query = ""
              }
            , Cmd.none
            )
                |> updateGamesFromProfiles toMsg
                |> updateGameProgress toMsg

        ReceiveProfile (Result.Err err) ->
            ( { model | status = Just <| HttpError err }
            , Cmd.none
            )

        RemoveProfile steamId64 ->
            ( { model | profiles = Dict.remove steamId64 model.profiles }
            , Cmd.none
            )
                |> updateGamesFromProfiles toMsg

        --
        -- GAMES
        --
        ReceiveGames appIds (Err err) ->
            -- @TODO: Fail instead of requeue to avoid infinite repeating
            ( { model
                | gameProgress = RemoteSet.requeueLoading appIds model.gameProgress
                , status = Just (HttpError err)
              }
            , Cmd.none
            )
                |> updateGameProgress toMsg

        ReceiveGames appIds (Ok gameResults) ->
            let
                importGames : List Steam.GameResult -> Model -> Model
                importGames games model_ =
                    case games of
                        [] ->
                            model_

                        Steam.GameParseError :: xs ->
                            importGames xs model_

                        (Steam.GameFound details) :: xs ->
                            importGames xs
                                { model_
                                    | gameDetails = Dict.insert details.appId details model_.gameDetails
                                    , gameProgress = RemoteSet.succeed [ details.appId ] model_.gameProgress
                                }

                        (Steam.GameRemoved appId) :: xs ->
                            importGames xs
                                { model_
                                    | gameProgress = RemoteSet.succeed [ appId ] model_.gameProgress
                                }

                requeueMissed : Model -> Model
                requeueMissed model_ =
                    { model_
                        | gameProgress = RemoteSet.requeueLoading appIds model_.gameProgress
                    }
            in
            ( model
                |> importGames gameResults
                |> requeueMissed
            , Cmd.none
            )
                |> updateGamesFromProfiles toMsg
                |> updateGameProgress toMsg

        --
        -- SUBMESSAGES
        --
        FiltersMsg subMsg ->
            let
                filters =
                    Filters.update FiltersMsg subMsg model.filters
            in
            ( { model | filters = filters }
            , Cmd.none
            )


updateGamesFromProfiles : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateGamesFromProfiles toMsg ( model, cmd ) =
    let
        allGameIds : List Steam.AppId
        allGameIds =
            model.profiles
                |> Dict.toList
                |> List.concatMap (\( _, profile ) -> Dict.keys profile.games)
    in
    ( model
        -- Handle discovery of new games
        |> (\m ->
                { m
                    | gameProgress = RemoteSet.queue allGameIds m.gameProgress
                }
           )
        -- Handle game removal (profile removed)
        |> (\m ->
                { m
                    | gameProgress = RemoteSet.whitelist allGameIds m.gameProgress
                }
           )
    , cmd
    )


updateGameProgress : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateGameProgress toMsg ( model, cmd ) =
    -- @TODO: batch-load next batch size of items
    -- Repeat until no more queued
    ( model, cmd )



--updateGameMatching : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
--updateGameMatching toMsg ( model, cmd ) =
--    { model
--        | matching = Filters.match model.filters model.profiles model.gameDetails
--    }
{-
   onGameProfileChange : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
   onGameProfileChange toMsg =
       let
           checkForGameActions : ( Model, Cmd msg ) -> ( Model, Cmd msg )
           checkForGameActions ( model, cmd ) =
               let
                   profileAppIds : List Steam.AppId
                   profileAppIds =
                       model.profiles
                           |> Dict.values
                           |> List.concatMap (.games >> Dict.keys)

                   games : Dict Steam.AppId (RemoteResult Http.Error Steam.AppId Steam.Game)
                   games =
                       profileAppIds
                           |> List.map toGame
                           |> Dict.fromList

                   toGame : Steam.AppId -> ( Steam.AppId, RemoteResult Http.Error Steam.AppId Steam.Game )
                   toGame appId =
                       ( appId
                       , Dict.get appId model.games
                           |> Maybe.withDefault (RemoteResult.Queued appId)
                       )

                   nextLoadIds : List Steam.AppId
                   nextLoadIds =
                       games
                           |> Dict.filter (\_ gameRes -> RemoteResult.isQueued gameRes)
                           |> Dict.keys
                           |> List.take batchSize
               in
               case nextLoadIds of
                   [] ->
                       ( { model | games = games }
                       , cmd
                       )

                   _ ->
                       let
                           pendingGameStates : Dict Steam.AppId (RemoteResult Http.Error Steam.AppId Steam.Game)
                           pendingGameStates =
                               nextLoadIds
                                   |> List.map (\id -> ( id, RemoteResult.Loading id ))
                                   |> Dict.fromList
                       in
                       ( { model | games = games }
                           |> bulkUpdateGames nextLoadIds pendingGameStates
                       , Cmd.batch
                           [ cmd
                           , Steam.loadGames nextLoadIds (ReceiveGames nextLoadIds)
                               |> Cmd.map toMsg
                           ]
                       )

           refreshFilters : ( Model, Cmd msg ) -> ( Model, Cmd msg )
           refreshFilters ( model, cmd ) =
               let
                   filters =
                       Filters.refresh
                           FiltersMsg
                           (Dict.values model.profiles)
                           (Dict.values model.games |> RemoteResult.oks)
                           model.filters
               in
               ( { model | filters = filters }
               , cmd
               )
       in
       checkForGameActions
           >> refreshFilters
-}


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
    H.div
        [ At.class "content game-grid"
        ]
        [ H.h2 []
            [ H.text "1. Add profiles" ]
        , profileManagerView model
        , H.h2 []
            [ H.text "2. Select filters" ]
        , Filters.view model.filters |> H.map FiltersMsg
        , H.h2 []
            [ H.text "3. Find the games" ]
        , gameListView model
        ]
        |> H.map toMsg


profileManagerView : Model -> Html Msg
profileManagerView model =
    let
        inputView : Maybe Status -> Html Msg
        inputView status =
            H.div
                [ At.class "profile-input" ]
                [ H.input
                    [ Ev.onInput OnInput
                    , At.value model.query
                    , Util.onKeyCodeDown OnKeyCodeDown
                    , At.disabled (status == Just ProfilePending)
                    ]
                    []
                , H.button
                    [ Ev.onClick LoadProfile ]
                    [ H.text "LATAA" ]
                ]

        statusView : Maybe Status -> Html msg
        statusView status =
            case status of
                Just (HttpError err) ->
                    H.div
                        [ At.class "error" ]
                        [ H.text <| "Got error :( " ++ Util.httpErrorTostring err ]

                Just DuplicateProfileError ->
                    H.div
                        [ At.class "warning" ]
                        [ H.text "Profile is already added!" ]

                Just ProfilePending ->
                    H.div
                        [ At.class "info" ]
                        [ H.text "Loading profile..." ]

                Just (Info msg) ->
                    H.div
                        [ At.class "info" ]
                        [ H.text msg ]

                Nothing ->
                    H.text ""

        profileListView : List Steam.Profile -> Html Msg
        profileListView =
            List.map profileView >> H.div []

        profileView : Steam.Profile -> Html Msg
        profileView profile =
            H.div
                [ At.class "profile" ]
                [ H.span
                    [ At.class "steam-id" ]
                    [ H.text profile.steamId ]
                , H.img
                    [ At.class "avatar-icon"
                    , At.src profile.avatarIcon
                    ]
                    []
                , H.span
                    [ At.class "game-count" ]
                    [ H.text <| String.fromInt (Dict.size profile.games) ++ " games" ]
                , H.button
                    [ At.class "remove"
                    , Ev.onClick (RemoveProfile profile.steamId64)
                    ]
                    [ H.text "remove" ]
                ]
    in
    H.div
        [ At.class "profile-manager" ]
        [ inputView model.status
        , statusView model.status
        , profileListView (Dict.values model.profiles)
        ]


gameListView : Model -> Html Msg
gameListView model =
    let
        loadProgress : Html Msg
        loadProgress =
            let
                countTotal =
                    RemoteSet.countTotal model.gameProgress

                progressPortion : ( String, String, Int ) -> Html msg
                progressPortion ( label, color, count ) =
                    let
                        percentage =
                            toFloat count * 100 / toFloat countTotal
                    in
                    H.div
                        [ At.class ("progress-portion progress-portion-" ++ label)
                        , At.style "display" "inline-block"
                        , At.style "background" color
                        , At.title label
                        , At.style "width" (String.fromFloat percentage ++ "%")
                        , At.style "overflow" "hidden"
                        ]
                        [ H.span [] [ H.text (String.fromInt count) ] ]

                stats =
                    [ ( "succeeded", "#9f9", RemoteSet.countSucceeded model.gameProgress )
                    , ( "failed", "#f99", RemoteSet.countFailed model.gameProgress )
                    , ( "queued", "#9ff", RemoteSet.countQueued model.gameProgress )
                    , ( "loading", "#999", RemoteSet.countLoading model.gameProgress )
                    ]
            in
            H.div
                [ At.class "load-progress progressbar"
                , At.style "background" "#eee"
                ]
                (List.map progressPortion stats)

        matchingGameRow : Steam.GameDetails -> Html Msg
        matchingGameRow game =
            let
                iconSrc =
                    --game.icon
                    "https://steamcdn-a.akamaihd.net/steam/apps/" ++ String.fromInt game.appId ++ "/capsule_sm_120.jpg"
            in
            H.tr [ At.class "game game-ok" ]
                [ H.td [] [ H.text (String.fromInt game.appId) ]
                , H.td [] [ H.img [ At.src iconSrc ] [] ]
                , H.td [] [ H.text game.name ]
                , H.td [] [ game.features |> String.join ", " |> H.text ]
                , H.td [] [ game.genres |> String.join ", " |> H.text ]
                ]

        matcher : Steam.GameDetails -> Bool
        matcher =
            Filters.match model.filters (Dict.values model.profiles)
    in
    H.div
        [ At.class "game-list" ]
        [ loadProgress
        , H.table
            [ At.class "game-table"
            ]
            (Dict.values model.gameDetails
                |> List.filter matcher
                |> List.map matchingGameRow
            )
        ]
