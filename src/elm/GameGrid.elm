module Page.GameGrid exposing
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
import Steam


type Status
    = HttpError Http.Error
    | ProfilePending
    | DuplicateProfileError


type alias Model =
    { profiles : List Steam.Profile
    , gameDetails : Dict Steam.AppId (RemoteResult Http.Error Steam.GameDetails)
    , profileInput : String
    , status : Maybe Status
    }


init : Model
init =
    { profiles = []
    , gameDetails = Dict.empty
    , profileInput = ""
    , status = Nothing
    }


type Msg
    = LoadProfile
    | GotProfile (Result Http.Error Steam.Profile)
    | ProfileInput String


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        ProfileInput profileInput ->
            ( { model | profileInput = profileInput }
            , Cmd.none
            )

        LoadProfile ->
            ( { model
                | status = Just ProfilePending
              }
            , Steam.loadProfile model.profileInput GotProfile
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
        , case model.status of
            Just ProfilePending ->
                H.text "LOADING"

            _ ->
                H.input
                    [ Ev.onInput ProfileInput ]
                    []
        , H.button
            [ Ev.onClick LoadProfile
            ]
            [ H.text "LATAA"
            ]
        ]
        |> H.map toMsg
