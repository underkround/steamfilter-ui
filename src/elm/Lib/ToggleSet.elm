module Lib.ToggleSet exposing
    ( ToggleSet
    , anySelected
    , clearSelected
    , empty
    , getAvailable
    , getSelected
    , isAvailable
    , isEmpty
    , isSelected
    , mapToList
    , setAvailable
    , toList
    , toggle
    )

import Lib.Util as Util
import Set exposing (Set)


type alias ToggleSet a =
    { available : Set a
    , selected : Set a
    }


empty : ToggleSet comparable
empty =
    { available = Set.empty
    , selected = Set.empty
    }


{-| Are there any available options? |
-}
isEmpty : ToggleSet comparable -> Bool
isEmpty =
    .available >> Set.isEmpty


clearSelected : ToggleSet comparable -> ToggleSet comparable
clearSelected set =
    { set | selected = Set.empty }


getAvailable : ToggleSet comparable -> List comparable
getAvailable set =
    Set.toList set.available


getSelected : ToggleSet comparable -> List comparable
getSelected set =
    Set.toList set.selected


{-| Are there any filters currently selected? |
-}
anySelected : ToggleSet comparable -> Bool
anySelected =
    .selected >> Set.isEmpty >> not


toggle : comparable -> ToggleSet comparable -> ToggleSet comparable
toggle x set =
    if Set.member x set.selected then
        { set
            | selected = Set.remove x set.selected
        }

    else if Set.member x set.available then
        { set
            | selected = Set.insert x set.selected
        }

    else
        set


toList : ToggleSet comparable -> List ( comparable, Bool )
toList set =
    Set.toList set.available
        |> List.map (\x -> ( x, isSelected set x ))


mapToList : (( comparable, Bool ) -> a) -> ToggleSet comparable -> List a
mapToList f =
    toList >> List.map f


isAvailable : ToggleSet comparable -> comparable -> Bool
isAvailable set =
    Util.flip Set.member set.available


isSelected : ToggleSet comparable -> comparable -> Bool
isSelected =
    .selected >> Util.flip Set.member


allSelected : ToggleSet comparable -> List comparable -> Bool
allSelected predicate =
    List.all (Util.flip Set.member predicate.selected)


setAvailable : List comparable -> ToggleSet comparable -> ToggleSet comparable
setAvailable availableList set =
    let
        available =
            Set.fromList availableList
    in
    { set
        | available = available
        , selected = Set.intersect set.selected available
    }
