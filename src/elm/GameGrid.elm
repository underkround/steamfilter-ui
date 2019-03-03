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


type alias Model =
    { profiles : List Steam.Profile
    , gameDetails : Dict Steam.AppId (RemoteResult Http.Error Steam.GameDetails)
    , query : String
    , status : Maybe Status
    }


init : Model
init =
    { profiles = []
    , gameDetails = Dict.empty
    , query = ""
    , status = Nothing
    }


type Msg
    = LoadProfile
    | GotProfile (Result Http.Error Steam.Profile)
    | OnInput String
    | OnKeyCodeDown Int


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
                    if List.member profile model.profiles then
                        ( Just DuplicateProfileError
                        , model.profiles
                        )

                    else
                        ( Nothing
                        , profile :: model.profiles
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
            ( { model
                | status = Just <| HttpError err
              }
            , Cmd.none
            )


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

        profileListView : List Steam.Profile -> Html msg
        profileListView =
            List.map profileView >> H.div []

        profileView : Steam.Profile -> Html msg
        profileView profile =
            H.div
                [ At.class "profile" ]
                [ H.text <| String.fromInt profile.steamId64 ]

        statusView : Maybe Status -> Html msg
        statusView status =
            case status of
                Just (HttpError err) ->
                    H.text <| "Got error :( " ++ Util.httpErrorTostring err

                Just DuplicateProfileError ->
                    H.text "Profile is already added!"

                Just ProfilePending ->
                    H.text "Loading profile..."

                Nothing ->
                    H.text ""
    in
    H.div
        [ At.class "profile-manager" ]
        [ inputView model.status
        , statusView model.status
        , profileListView model.profiles
        ]
