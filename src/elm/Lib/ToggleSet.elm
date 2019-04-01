module Lib.ToggleSet exposing
    ( ToggleSet
    , anySelected
    , empty
    , getAvailable
    , getSelected
    , getWeight
    , isAvailable
    , isEmpty
    , isSelected
    , mapToBoolList
    , mapToList
    , setAvailablesGrouped
    , setAvailablesWeighted
    , toBoolList
    , toList
    , toggle
    , unsetAll
    )

import Dict exposing (Dict)
import Lib.Util as Util
import List.Extra
import Set exposing (Set)


type alias ToggleSet a =
    Dict a Flags


type alias Flags =
    { selected : Bool
    , weight : Int
    }


empty : ToggleSet comparable
empty =
    Dict.empty


{-| Update availables, setting weight by the times item appears in the list |
-}
setAvailablesGrouped : List String -> ToggleSet String -> ToggleSet String
setAvailablesGrouped availablesList oldSet =
    let
        --fromGrouped : ( comparable, List comparable ) -> ( comparable, Flags )
        fromGrouped ( key, grouped ) =
            ( key
            , { selected = isSelected key oldSet
              , weight = List.length grouped + 1
              }
            )
    in
    availablesList
        |> List.sort
        |> List.Extra.group
        |> List.map fromGrouped
        |> Dict.fromList


setAvailablesWeighted : List ( comparable, Int ) -> ToggleSet comparable -> ToggleSet comparable
setAvailablesWeighted availablesWeightList oldSet =
    let
        fromWeighted : ( comparable, Int ) -> ( comparable, Flags )
        fromWeighted ( key, weight ) =
            ( key
            , { selected = isSelected key oldSet
              , weight = weight
              }
            )
    in
    availablesWeightList
        |> List.map fromWeighted
        |> Dict.fromList


{-| Are there any available options? |
-}
isEmpty : ToggleSet comparable -> Bool
isEmpty =
    Dict.isEmpty


unsetAll : ToggleSet comparable -> ToggleSet comparable
unsetAll =
    Dict.map (\_ flags -> { flags | selected = False })


getAvailable : ToggleSet comparable -> List comparable
getAvailable =
    Dict.keys


getSelected : ToggleSet comparable -> List comparable
getSelected =
    Dict.filter (\_ flags -> flags.selected)
        >> Dict.keys


{-| Are there any filters currently selected? |
-}
anySelected : ToggleSet comparable -> Bool
anySelected =
    Dict.filter (\_ flags -> flags.selected)
        >> Dict.isEmpty
        >> not


toggle : comparable -> ToggleSet comparable -> ToggleSet comparable
toggle key set =
    let
        toggler : Maybe Flags -> Maybe Flags
        toggler =
            Maybe.map (\flags -> { flags | selected = not flags.selected })
    in
    Dict.update key toggler set


toList : ToggleSet comparable -> List ( comparable, Bool, Int )
toList =
    let
        mapper : comparable -> Flags -> ( comparable, Bool, Int )
        mapper key flags =
            ( key, flags.selected, flags.weight )
    in
    Dict.map mapper >> Dict.values


toBoolList : ToggleSet comparable -> List ( comparable, Bool )
toBoolList =
    let
        mapper : comparable -> Flags -> ( comparable, Bool )
        mapper key flags =
            ( key, flags.selected )
    in
    Dict.map mapper >> Dict.values


mapToList : (( comparable, Bool, Int ) -> a) -> ToggleSet comparable -> List a
mapToList f =
    toList >> List.map f


mapToBoolList : (( comparable, Bool ) -> a) -> ToggleSet comparable -> List a
mapToBoolList f =
    toBoolList >> List.map f


isAvailable : comparable -> ToggleSet comparable -> Bool
isAvailable =
    Dict.member


isSelected : comparable -> ToggleSet comparable -> Bool
isSelected key set =
    Dict.get key set
        |> Maybe.map .selected
        |> Maybe.withDefault False


getWeight : comparable -> ToggleSet comparable -> Int
getWeight key set =
    Dict.get key set
        |> Maybe.map .weight
        |> Maybe.withDefault 0
