module Lib.RemoteResult exposing
    ( RemoteResult(..)
    , isErr
    , isInitial
    , isOk
    , isPending
    , isReceived
    , map
    )


type RemoteResult err a
    = Initial
    | Pending
    | Err err
    | Ok a


map : (a -> b) -> RemoteResult err a -> RemoteResult err b
map fOk =
    mapBoth identity fOk


mapErr : (err -> err2) -> RemoteResult err a -> RemoteResult err2 a
mapErr fErr =
    mapBoth fErr identity


mapBoth : (err -> err2) -> (a -> b) -> RemoteResult err a -> RemoteResult err2 b
mapBoth fErr fOk remote =
    case remote of
        Initial ->
            Initial

        Pending ->
            Pending

        Err err ->
            Err (fErr err)

        Ok a ->
            Ok (fOk a)


isInitial : RemoteResult err a -> Bool
isInitial remote =
    case remote of
        Initial ->
            True

        _ ->
            False


isPending : RemoteResult err a -> Bool
isPending remote =
    case remote of
        Pending ->
            True

        _ ->
            False


isErr : RemoteResult err a -> Bool
isErr remote =
    case remote of
        Err _ ->
            True

        _ ->
            False


isOk : RemoteResult err a -> Bool
isOk remote =
    case remote of
        Ok _ ->
            True

        _ ->
            False


isReceived : RemoteResult err a -> Bool
isReceived remote =
    isOk remote || isErr remote
