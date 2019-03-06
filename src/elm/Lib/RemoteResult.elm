module Lib.RemoteResult exposing
    ( RemoteResult(..)
    , isErr
    , isLoading
    , isOk
    , isQueued
    , isReceived
    , map
    , oks
    )


type RemoteResult err k a
    = Queued k
    | Loading k
    | Err err
    | Ok a


oks : List (RemoteResult err k a) -> List a
oks results =
    case results of
        [] ->
            []

        (Ok x) :: xs ->
            x :: oks xs

        _ :: xs ->
            oks xs


map : (a -> b) -> RemoteResult err k a -> RemoteResult err k b
map f remote =
    case remote of
        Queued k ->
            Queued k

        Loading k ->
            Loading k

        Err err ->
            Err err

        Ok a ->
            Ok (f a)


mapErr : (err -> err2) -> RemoteResult err k a -> RemoteResult err2 k a
mapErr f remote =
    case remote of
        Queued k ->
            Queued k

        Loading k ->
            Loading k

        Err err ->
            Err (f err)

        Ok a ->
            Ok a


isQueued : RemoteResult err k a -> Bool
isQueued remote =
    case remote of
        Queued _ ->
            True

        _ ->
            False


isLoading : RemoteResult err k a -> Bool
isLoading remote =
    case remote of
        Loading _ ->
            True

        _ ->
            False


isErr : RemoteResult err k a -> Bool
isErr remote =
    case remote of
        Err _ ->
            True

        _ ->
            False


isOk : RemoteResult err k a -> Bool
isOk remote =
    case remote of
        Ok _ ->
            True

        _ ->
            False


isReceived : RemoteResult err k a -> Bool
isReceived remote =
    isOk remote || isErr remote
