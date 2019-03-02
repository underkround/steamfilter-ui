module Steam.Types exposing
    ( AppId
    , GameDetails
    , Profile
    , ProfileGameDetails
    )

import Dict exposing (Dict)
import Lib.Remote as Remote exposing (Remote)


type alias Profile =
    { steamId64 : Int
    , steamId : String
    , games : Dict AppId ProfileGameDetails
    }


type alias AppId =
    Int


type alias ProfileGameDetails =
    { appId : AppId
    , hoursLast2Weeks : Float
    , hoursOnRecord : Int
    , statsLink : String
    }


type alias GameDetails =
    { appId : AppId
    , name : String
    , logo : String
    , storeLink : String
    , globalStatsLink : String
    }
