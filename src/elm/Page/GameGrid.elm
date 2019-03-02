module Page.GameGrid exposing
    ( State
    , init
    , view
    )

import Dict exposing (Dict)
import Html as H exposing (Html)
import Lib.Remote as Remote exposing (Remote)
import Steam.Types as ST


type alias State =
    { profiles : Dict String (Remote (Result String ST.Profile))
    , gameDetails : Dict ST.AppId (Remote (Result String ST.GameDetails))
    }


init : State
init =
    { profiles = Dict.empty
    , gameDetails = Dict.empty
    }


view : State -> Html msg
view state =
    H.text "@TODO: gamegrid"
