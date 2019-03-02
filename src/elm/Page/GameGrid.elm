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
import Steam


type alias Model =
    { profiles : Dict String (Remote (Result String Steam.Profile))
    , gameDetails : Dict Steam.AppId (Remote (Result String Steam.GameDetails))
    , profileInput : String
    }


init : Model
init =
    { profiles = Dict.empty
    , gameDetails = Dict.empty
    , profileInput = ""
    }


type Msg
    = LoadProfile
    | GotProfile String (Result Http.Error Steam.Profile)
    | ProfileInput String


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        ProfileInput profileInput ->
            ( { model | profileInput = profileInput }
            , Cmd.none
            )

        LoadProfile ->
            ( model
            , Steam.loadProfile model.profileInput (GotProfile model.profileInput)
                |> Cmd.map toMsg
            )

        GotProfile profileId (Result.Err err) ->
            Debug.log (profileId ++ " -> error") err
                |> always
                    ( model, Cmd.none )

        GotProfile profileId (Result.Ok ok) ->
            Debug.log (profileId ++ " -> ok") ok
                |> always
                    ( model, Cmd.none )


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
    H.div
        [ At.class "content game-grid"
        ]
        [ H.h2 [] [ H.text "@TODO: gamegrid" ]
        , H.input
            [ Ev.onInput ProfileInput
            ]
            []
        , H.button
            [ Ev.onClick LoadProfile
            ]
            [ H.text "LATAA"
            ]
        ]
        |> H.map toMsg
