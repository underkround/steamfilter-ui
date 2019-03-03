module GameGrid exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Http
import Lib.Remote as Remote exposing (Remote)
import Lib.RemoteResult as RemoteResult exposing (RemoteResult)
import Lib.Util as Util
import Steam


type Status
    = HttpError Http.Error
    | ProfilePending
    | DuplicateProfileError
    | Info String


type alias Model =
    { profiles : Dict String Steam.Profile
    , games : Dict Steam.AppId (RemoteResult Http.Error Steam.Game)
    , query : String
    , status : Maybe Status
    }


batchSize : Int
batchSize =
    3


init : Model
init =
    { profiles = Dict.empty
    , games = Dict.empty
    , query = ""
    , status = Nothing
    }


type Msg
    = LoadProfile
    | ReceiveProfile (Result Http.Error Steam.Profile)
    | ReceiveGames (List Steam.AppId) (Result Http.Error (List Steam.Game))
    | OnInput String
    | OnKeyCodeDown Int
    | RemoveProfile String


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
                |> checkForGameActions toMsg

        ReceiveProfile (Result.Err err) ->
            ( { model | status = Just <| HttpError err }
            , Cmd.none
            )

        ReceiveGames appIds (Ok games) ->
            let
                updatedGames =
                    gameListToDict games
                        |> Dict.map (\id game -> RemoteResult.Ok game)
            in
            ( model
                |> bulkUpdateGames appIds updatedGames
            , Cmd.none
            )
                |> checkForGameActions toMsg

        ReceiveGames appIds (Result.Err err) ->
            ( model
                |> bulkUpdateGames appIds Dict.empty
            , Cmd.none
            )
                |> checkForGameActions toMsg

        RemoveProfile steamId64 ->
            if Dict.member steamId64 model.profiles then
                ( { model | profiles = Dict.remove steamId64 model.profiles }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


gameListToDict : List Steam.Game -> Dict Steam.AppId Steam.Game
gameListToDict =
    let
        toPair : Steam.Game -> ( Steam.AppId, Steam.Game )
        toPair game =
            ( game.appId, game )
    in
    List.map toPair >> Dict.fromList


bulkUpdateGames : List Steam.AppId -> Dict Steam.AppId (RemoteResult Http.Error Steam.Game) -> Model -> Model
bulkUpdateGames targetAppIds receivedGames model =
    case targetAppIds of
        [] ->
            model

        appId :: xs ->
            let
                updatedGame : RemoteResult Http.Error Steam.Game
                updatedGame =
                    Dict.get appId receivedGames
                        |> Maybe.withDefault (RemoteResult.Err (Http.BadStatus 404))
            in
            bulkUpdateGames xs receivedGames { model | games = Dict.insert appId updatedGame model.games }


checkForGameActions : (Msg -> msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
checkForGameActions toMsg ( model, cmd ) =
    let
        profileAppIds : List Steam.AppId
        profileAppIds =
            model.profiles
                |> Dict.values
                |> List.concatMap (.games >> Dict.keys)

        games : Dict Steam.AppId (RemoteResult Http.Error Steam.Game)
        games =
            profileAppIds
                |> List.map toGame
                |> Dict.fromList

        toGame : Steam.AppId -> ( Steam.AppId, RemoteResult Http.Error Steam.Game )
        toGame appId =
            ( appId
            , Dict.get appId model.games
                |> Maybe.withDefault RemoteResult.Initial
            )

        nextLoadIds : List Steam.AppId
        nextLoadIds =
            games
                |> Dict.filter (\_ gameRes -> RemoteResult.isInitial gameRes)
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
                pendingGameStates : Dict Steam.AppId (RemoteResult Http.Error Steam.Game)
                pendingGameStates =
                    nextLoadIds
                        |> List.map (\id -> ( id, RemoteResult.Pending ))
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
        , gameFilterView model
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
                totalCount =
                    model.games
                        |> Dict.size

                okCount =
                    model.games
                        |> Dict.filter (\key gameRes -> RemoteResult.isOk gameRes)
                        |> Dict.size

                errCount =
                    model.games
                        |> Dict.filter (\key gameRes -> RemoteResult.isErr gameRes)
                        |> Dict.size

                okPercentage =
                    okCount * 100 // totalCount

                errPercentage =
                    errCount * 100 // totalCount
            in
            H.div
                [ At.class "load-progress progressbar"
                , At.style "background" "#eee"
                ]
                [ H.div
                    [ At.class "progress-portion progress-portion-failed"
                    , At.style "display" "inline-block"
                    , At.style "background" "#faa"
                    , At.style "width" (String.fromInt errPercentage ++ "%")
                    , At.style "overflow" "hidden"
                    ]
                    [ H.span [] [ H.text (String.fromInt errCount) ] ]
                , H.div
                    [ At.class "progress-portion progress-portion-ok"
                    , At.style "display" "inline-block"
                    , At.style "background" "#afa"
                    , At.style "width" (String.fromInt okPercentage ++ "%")
                    , At.style "overflow" "hidden"
                    ]
                    [ H.span [] [ H.text (String.fromInt okCount) ] ]
                ]

        gameView : ( Steam.AppId, RemoteResult Http.Error Steam.Game ) -> Html Msg
        gameView ( appId, gameResult ) =
            case gameResult of
                RemoteResult.Initial ->
                    H.tr []
                        [ H.td [] [ H.text (String.fromInt appId) ]
                        , H.td [ At.colspan 2 ] [ H.text "Queued..." ]
                        ]

                RemoteResult.Pending ->
                    H.tr []
                        [ H.td [] [ H.text (String.fromInt appId) ]
                        , H.td [ At.colspan 2 ] [ H.text "Loading..." ]
                        ]

                RemoteResult.Err _ ->
                    H.tr []
                        [ H.td [] [ H.text (String.fromInt appId) ]
                        , H.td [ At.colspan 2 ] [ H.text "Error :(" ]
                        ]

                RemoteResult.Ok game ->
                    H.tr []
                        [ H.td [] [ H.text (String.fromInt game.appId) ]
                        , H.td [] [ H.img [ At.src game.icon ] [] ]
                        , H.td [] [ H.text game.name ]
                        ]
    in
    H.div
        [ At.class "game-list" ]
        [ loadProgress
        , H.table
            [ At.class "game-table"
            ]
            (model.games
                |> Dict.toList
                |> List.map gameView
            )
        ]


gameFilterView : Model -> Html Msg
gameFilterView model =
    H.text "@TODO gameFilterView"
