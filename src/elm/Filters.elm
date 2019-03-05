module Filters exposing
    ( Filters
    , Msg
    , anySelected
    , init
    , match
    , refresh
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Lib.ToggleSet as ToggleSet exposing (ToggleSet)
import Lib.Util as Util
import List.Extra
import Set exposing (Set)
import Set.Extra
import Steam


type alias Filters =
    { features : ToggleSet String
    , genres : ToggleSet String
    , owners : ToggleSet String
    }


init : Filters
init =
    { features = ToggleSet.empty
    , genres = ToggleSet.empty
    , owners = ToggleSet.empty
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


match : Filters -> List Steam.Profile -> Steam.Game -> Bool
match filters profiles game =
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
    in
    Util.allPredicates
        [ .features
            >> Set.fromList
            >> requireAll (ToggleSet.getSelected filters.features)
        , .genres
            >> Set.fromList
            >> requireAll (ToggleSet.getSelected filters.genres)

        --, @TODO: owners
        ]
        game


{-| Calculate new filters from souce data while keeping valid selections
-}
refresh : (Msg -> msg) -> List Steam.Profile -> List Steam.Game -> Filters -> Filters
refresh toMsg profiles games filters =
    { features = ToggleSet.setAvailablesGrouped (List.concatMap .features games) filters.features
    , genres = ToggleSet.setAvailablesGrouped (List.concatMap .genres games) filters.genres
    , owners = ToggleSet.setAvailablesGrouped (List.map .steamId profiles) filters.owners
    }


type Msg
    = ToggleFeature String
    | ToggleGenre String
    | ToggleOwner Steam.SteamId64


update : (Msg -> msg) -> Msg -> Filters -> Filters
update toMsg msg filters =
    case msg of
        ToggleFeature feature ->
            { filters | features = ToggleSet.toggle feature filters.features }

        ToggleGenre genre ->
            { filters | genres = ToggleSet.toggle genre filters.genres }

        ToggleOwner owner ->
            { filters | owners = ToggleSet.toggle owner filters.owners }


view : Filters -> Html Msg
view filters =
    let
        toggle : (String -> Msg) -> ( String, Bool, Int ) -> Html Msg
        toggle msg ( value, selected, weight ) =
            H.button
                [ At.classList
                    [ ( "toggle", True )
                    , ( "selected", selected )
                    ]
                , Ev.onClick (msg value)
                ]
                [ H.span [ At.class "value" ]
                    [ H.text value ]
                , H.span [ At.class "weight" ]
                    [ H.text <| " (" ++ String.fromInt weight ++ ")" ]
                ]
    in
    H.div
        [ At.class "filters" ]
        [ H.h3 [] [ H.text "Owners" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggle ToggleOwner) filters.owners)
        , H.h3 [] [ H.text "Features" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggle ToggleFeature) filters.features)
        , H.h3 [] [ H.text "Genres" ]
        , H.div
            [ At.class "filter-set" ]
            (ToggleSet.mapToList (toggle ToggleGenre) filters.genres)
        ]
