module Filters exposing
    ( Filters
    , Msg
    , anySelected
    , getGames
    , init
    , process
    , update
    , view
    )

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Lib.ToggleSet as ToggleSet exposing (ToggleSet)
import Lib.Util as Util
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Set.Extra
import Steam


type alias Filters =
    { features : ToggleSet String
    , genres : ToggleSet String
    , owners : ToggleSet Steam.SteamId64

    -- Store matching games for optimization
    , matching : List Steam.GameDetails
    }


init : Filters
init =
    { features = ToggleSet.empty
    , genres = ToggleSet.empty
    , owners = ToggleSet.empty
    , matching = []
    }


{-| Are there any filters currently active? |
-}
anySelected : Filters -> Bool
anySelected filters =
    [ filters.features
    , filters.genres
    , filters.owners
    ]
        |> List.any ToggleSet.anySelected


getGames : Filters -> List Steam.GameDetails
getGames =
    .matching


match : Filters -> List (Set Steam.AppId) -> Steam.GameDetails -> Bool
match filters selectedProfileGames game =
    let
        requireAll : List String -> Set String -> Bool
        requireAll required having =
            case required of
                [] ->
                    True

                x :: xs ->
                    if Set.member x having then
                        requireAll xs having

                    else
                        False

        memberOfAll : List (Set Steam.AppId) -> Steam.AppId -> Bool
        memberOfAll sets appId =
            case sets of
                [] ->
                    True

                set :: xs ->
                    if Set.member appId set then
                        memberOfAll xs appId

                    else
                        False
    in
    Util.allPredicates
        [ .features
            >> Set.fromList
            >> requireAll (ToggleSet.getSelected filters.features)
        , .genres
            >> Set.fromList
            >> requireAll (ToggleSet.getSelected filters.genres)
        , .appId
            >> memberOfAll selectedProfileGames
        ]
        game


{-| Calculate new filters from souce data while keeping valid selections
-}
process : Dict Steam.SteamId64 Steam.Profile -> List Steam.GameDetails -> Filters -> Filters
process profiles games filters =
    let
        selectedProfileGames : List (Set Steam.AppId)
        selectedProfileGames =
            ToggleSet.getSelected filters.owners
                |> List.map (Util.flip Dict.get profiles)
                |> Maybe.Extra.values
                |> List.map (.games >> Dict.keys >> Set.fromList)

        matchingGames : List Steam.GameDetails
        matchingGames =
            List.filter (match filters selectedProfileGames) games

        matchingGameIds : Set Steam.AppId
        matchingGameIds =
            List.map .appId matchingGames
                |> Set.fromList

        profileAndWeight : Steam.Profile -> ( Steam.SteamId64, Int )
        profileAndWeight profile =
            ( profile.steamId64
            , profile.games
                |> Dict.keys
                |> Set.fromList
                |> Set.intersect matchingGameIds
                |> Set.size
            )
    in
    { features =
        ToggleSet.setAvailablesGrouped
            (List.concatMap .features matchingGames)
            filters.features
    , genres =
        ToggleSet.setAvailablesGrouped
            (List.concatMap .genres matchingGames)
            filters.genres
    , owners =
        ToggleSet.setAvailablesWeighted
            (profiles |> Dict.values |> List.map profileAndWeight)
            filters.owners
    , matching = matchingGames
    }


type Msg
    = ToggleFeature String
    | ToggleGenre String
    | ToggleOwner Steam.SteamId64


update : Msg -> Filters -> Filters
update msg filters =
    case msg of
        ToggleFeature feature ->
            { filters | features = ToggleSet.toggle feature filters.features }

        ToggleGenre genre ->
            { filters | genres = ToggleSet.toggle genre filters.genres }

        ToggleOwner owner ->
            { filters | owners = ToggleSet.toggle owner filters.owners }


view : (Msg -> msg) -> (Steam.SteamId64 -> Maybe Steam.Profile) -> Filters -> Html msg
view toMsg getProfile filters =
    let
        toggleView : (a -> Html Msg) -> (a -> Msg) -> ( a, Bool, Int ) -> Html Msg
        toggleView valueView msg ( value, selected, weight ) =
            H.button
                [ At.classList
                    [ ( "toggle", True )
                    , ( "selected", selected )
                    ]
                , Ev.onClick (msg value)
                ]
                [ H.span [ At.class "value" ]
                    [ valueView value ]
                , H.span [ At.class "weight" ]
                    [ H.text <| " (" ++ String.fromInt weight ++ ")" ]
                ]

        profileView : Steam.SteamId64 -> Html Msg
        profileView =
            getProfile
                >> Maybe.map .steamId
                >> Maybe.withDefault ""
                >> H.text
    in
    H.div
        [ At.class "filters" ]
        [ H.h3 [] [ H.text "Owners" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggleView profileView ToggleOwner) filters.owners)
        , H.h3 [] [ H.text "Features" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggleView H.text ToggleFeature) filters.features)
        , H.h3 [] [ H.text "Genres" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggleView H.text ToggleGenre) filters.genres)
        ]
        |> H.map toMsg
