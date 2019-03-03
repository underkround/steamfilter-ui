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


init : Model
init =
    { profiles = Dict.empty
    , games = Dict.empty
    , query = ""
    , status = Nothing
    }


type Msg
    = LoadProfile
    | GotProfile (Result Http.Error Steam.Profile)
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
            , Steam.loadProfile model.query GotProfile
                |> Cmd.map toMsg
            )

        GotProfile (Ok profile) ->
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

        GotProfile (Result.Err err) ->
            ( { model | status = Just <| HttpError err }
            , Cmd.none
            )

        RemoveProfile steamId64 ->
            if Dict.member steamId64 model.profiles then
                ( { model | profiles = Dict.remove steamId64 model.profiles }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
    H.div
        [ At.class "content game-grid"
        ]
        [ H.h2 [] [ H.text "@TODO: gamegrid" ]
        , profileManagerView model
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
                [ H.text profile.steamId
                , H.text ("(" ++ String.fromInt (Dict.size profile.games) ++ " games)")
                , H.button
                    [ At.class "remove"
                    , Ev.onClick (RemoveProfile profile.steamId64)
                    ]
                    [ H.text "[x]" ]
                ]
    in
    H.div
        [ At.class "profile-manager" ]
        [ inputView model.status
        , statusView model.status
        , profileListView (Dict.values model.profiles)
        ]
