module GameGrid exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Browser.Dom
import Dict exposing (Dict)
import Filters exposing (Filters)
import GameSet exposing (GameSet)
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Http
import Lib.Util as Util
import Set exposing (Set)
import Steam
import Task


type Status
    = HttpError Http.Error
    | ProfilePending
    | DuplicateProfileError
    | Info String


type alias Model =
    { profiles : Dict Steam.SteamId64 Steam.Profile
    , gameSet : GameSet
    , matching : List Steam.GameDetails
    , query : String
    , status : Maybe Status
    , filters : Filters
    }


init : Model
init =
    { profiles = Dict.empty
    , gameSet = GameSet.init 30 3
    , matching = []
    , query = ""
    , status = Nothing
    , filters = Filters.init
    }


type Msg
    = NoOp
    | OnFocus (Result Browser.Dom.Error ())
    | OnInput String
    | OnKeyCodeDown Int
    | LoadProfile
    | ReceiveProfile (Result Http.Error Steam.Profile)
    | RemoveProfile String
    | GameSetMsg GameSet.Msg
    | FiltersMsg Filters.Msg


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnFocus (Ok x) ->
            ( model, Debug.log "focus:ok" x |> always Cmd.none )

        OnFocus (Err err) ->
            ( model, Debug.log "focus:err" err |> always Cmd.none )

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
                |> updateGameLoading toMsg
                |> updateFilters toMsg

        ReceiveProfile (Result.Err err) ->
            ( { model | status = Just <| HttpError err }
            , Cmd.none
            )

        RemoveProfile steamId64 ->
            ( { model | profiles = Dict.remove steamId64 model.profiles }
            , Cmd.none
            )
                |> updateGamesFromProfiles toMsg
                |> updateFilters toMsg

        --
        -- SUBMESSAGES
        --
        GameSetMsg subMsg ->
            GameSet.update (GameSetMsg >> toMsg) subMsg model.gameSet
                |> Tuple.mapFirst (\sub -> { model | gameSet = sub })
                |> updateFilters toMsg

        FiltersMsg subMsg ->
            ( { model
                | filters =
                    Filters.update subMsg model.filters
                        |> Filters.process
                            (Dict.values model.profiles)
                            (GameSet.getGames model.gameSet)
              }
            , Cmd.none
            )


updateGamesFromProfiles : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateGamesFromProfiles toMsg ( model, cmd ) =
    let
        allGameIds : List Steam.AppId
        allGameIds =
            model.profiles
                |> Dict.values
                |> List.concatMap (.games >> Dict.keys)
    in
    ( { model
        | gameSet = GameSet.queueAndWhitelist allGameIds model.gameSet
      }
    , cmd
    )


updateGameLoading : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateGameLoading toMsg ( model, cmd ) =
    -- @TODO: batch-load next batch size of items
    -- Repeat until no more queued
    GameSet.loadParallel (GameSetMsg >> toMsg) model.gameSet
        |> Tuple.mapFirst (\sub -> { model | gameSet = sub })
        |> Tuple.mapSecond (\sub -> Cmd.batch [ cmd, sub ])


updateFilters : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateFilters toMsg ( model, cmd ) =
    ( { model
        | filters =
            Filters.process
                (Dict.values model.profiles)
                (GameSet.getGames model.gameSet)
                model.filters
      }
    , cmd
    )


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
        , Filters.view FiltersMsg model.filters
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
                [ At.class "profile-input"
                , At.id "profile-input"
                ]
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
                stats : GameSet.Stats
                stats =
                    GameSet.stats model.gameSet

                progressPortion : ( String, String, Int ) -> Html msg
                progressPortion ( label, color, count ) =
                    let
                        percentage =
                            toFloat count / toFloat stats.total * 100
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

                statsList =
                    [ ( "missing", "#f33", stats.missing )
                    , ( "ok", "#9f9", stats.ok )
                    , ( "failed", "#f99", stats.failed )
                    , ( "queued", "#9ff", stats.queued )
                    , ( "loading", "#999", stats.loading )
                    ]
            in
            if GameSet.isEmpty model.gameSet then
                H.text ""

            else
                H.div
                    [ At.class "load-progress progressbar"
                    , At.style "background" "#eee"
                    ]
                    (List.map progressPortion statsList)

        gameRow : Steam.GameDetails -> Html Msg
        gameRow game =
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

        gamesToView : List Steam.GameDetails
        gamesToView =
            if Filters.anySelected model.filters then
                Filters.getGames model.filters

            else
                GameSet.getGames model.gameSet
    in
    H.div
        [ At.class "game-list" ]
        [ loadProgress
        , H.table
            [ At.class "game-table"
            ]
            (List.map gameRow gamesToView)
        ]
