module Lib.Remote exposing
    ( Remote(..)
    , isInitial
    , isPending
    , isReceived
    , map
    )


type Remote a
    = Initial
    | Pending
    | Received a


map : (a -> b) -> Remote a -> Remote b
map f remote =
    case remote of
        Initial ->
            Initial

        Pending ->
            Pending

        Received a ->
            Received (f a)


isInitial : Remote a -> Bool
isInitial remote =
    case remote of
        Initial ->
            True

        _ ->
            False


isPending : Remote a -> Bool
isPending remote =
    case remote of
        Pending ->
            True

        _ ->
            False


isReceived : Remote a -> Bool
isReceived remote =
    case remote of
        Received _ ->
            True

        _ ->
            False
