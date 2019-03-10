module Lib.RemoteSet exposing
    ( RemoteSet
    , countFailed
    , countLoading
    , countQueued
    , countSucceeded
    , countTotal
    , fail
    , hasFailed
    , hasLoading
    , hasQueued
    , hasSucceeded
    , init
    , load
    , queue
    , requeueAllFailed
    , requeueLoading
    , succeed
    , whitelist
    )

import Dict exposing (Dict)
import Set exposing (Set)


type alias RemoteSet err a =
    { queued : Set a
    , loading : Set a
    , succeeded : Set a
    , failed : Set a
    , errors : Dict a err
    , failCounts : Dict a Int
    , maxRetries : Int
    }


init : Int -> RemoteSet err a
init maxRetries =
    { queued = Set.empty
    , loading = Set.empty
    , succeeded = Set.empty
    , failed = Set.empty
    , errors = Dict.empty
    , failCounts = Dict.empty
    , maxRetries = maxRetries
    }


countFailed : RemoteSet err a -> Int
countFailed =
    Set.size << .failed


countLoading : RemoteSet err a -> Int
countLoading =
    Set.size << .loading


countQueued : RemoteSet err a -> Int
countQueued =
    Set.size << .queued


countSucceeded : RemoteSet err a -> Int
countSucceeded =
    Set.size << .succeeded


countTotal : RemoteSet err a -> Int
countTotal set =
    countFailed set
        + countLoading set
        + countQueued set
        + countSucceeded set


hasQueued : RemoteSet err a -> Bool
hasQueued =
    not << Set.isEmpty << .queued


hasLoading : RemoteSet err a -> Bool
hasLoading =
    not << Set.isEmpty << .loading


hasSucceeded : RemoteSet err a -> Bool
hasSucceeded =
    not << Set.isEmpty << .succeeded


hasFailed : RemoteSet err a -> Bool
hasFailed =
    not << Set.isEmpty << .failed


getQueued : RemoteSet err a -> Set a
getQueued =
    .queued


getLoading : RemoteSet err a -> Set a
getLoading =
    .loading


getSucceeded : RemoteSet err a -> Set a
getSucceeded =
    .succeeded


getFailed : RemoteSet err comparable -> List ( comparable, Maybe err, Int )
getFailed model =
    let
        mapper : comparable -> ( comparable, Maybe err, Int )
        mapper a =
            ( a
            , Dict.get a model.errors
            , Dict.get a model.failCounts |> Maybe.withDefault 0
            )
    in
    model.failed
        |> Set.toList
        |> List.map mapper


{-| Remove all items not matching the whitelist
-}
whitelist : List comparable -> RemoteSet err comparable -> RemoteSet err comparable
whitelist whitelistItems model =
    let
        whiteSet : Set comparable
        whiteSet =
            Set.fromList whitelistItems
    in
    { model
        | queued = Set.intersect whiteSet model.queued
        , loading = Set.intersect whiteSet model.loading
        , succeeded = Set.intersect whiteSet model.succeeded
        , failed = Set.intersect whiteSet model.failed

        -- @TODO: reset fail counts etc
    }


{-| Add items to queue to be loaded

Move things to queue waiting for loading
Entrypoint to the collection, so keep things unique!

-}
queue : List comparable -> RemoteSet err comparable -> RemoteSet err comparable
queue items model =
    let
        existing : Set comparable
        existing =
            Set.union model.loading <|
                Set.union model.succeeded
                    model.failed

        actuallyNewItems : Set comparable
        actuallyNewItems =
            Set.diff (Set.fromList items) existing
    in
    { model | queued = Set.union actuallyNewItems model.queued }


{-| Load next batch

Take from queued, move to loading, call action

-}
load :
    Int
    -> (List comparable -> Cmd msg)
    -> RemoteSet err comparable
    -> ( RemoteSet err comparable, Cmd msg, List comparable )
load batchSize loadAction model =
    let
        toLoadList : List comparable
        toLoadList =
            Set.toList model.queued
                |> List.take batchSize

        toLoadSet : Set comparable
        toLoadSet =
            Set.fromList toLoadList

        leftQueued : Set comparable
        leftQueued =
            Set.diff model.queued toLoadSet

        nowLoading : Set comparable
        nowLoading =
            Set.union model.loading toLoadSet
    in
    ( { model
        | queued = leftQueued
        , loading = nowLoading
      }
    , loadAction toLoadList
    , toLoadList
    )


{-| Mark loading items as succeed
-}
succeed : List comparable -> RemoteSet err comparable -> RemoteSet err comparable
succeed items model =
    case items of
        [] ->
            model

        x :: xs ->
            if Set.member x model.loading then
                -- It was actually loading
                succeed xs
                    { model
                        | loading = Set.remove x model.loading
                        , succeeded = Set.insert x model.succeeded
                        , errors = Dict.remove x model.errors
                        , failCounts = Dict.remove x model.failCounts
                    }

            else
                succeed xs model


{-| Requeue item that was loading (it never arrived?)
-}
requeueLoading : List comparable -> RemoteSet err comparable -> RemoteSet err comparable
requeueLoading items model =
    case items of
        [] ->
            model

        x :: xs ->
            if Set.member x model.loading then
                requeueLoading xs
                    { model
                        | loading = Set.remove x model.loading
                        , queued = Set.insert x model.queued
                    }

            else
                requeueLoading xs model


{-| Mark item as failed

Advanse the fail counter

-}
fail : List ( comparable, err ) -> RemoteSet err comparable -> RemoteSet err comparable
fail itemsAndErrors model =
    let
        countBumper : Maybe Int -> Maybe Int
        countBumper =
            Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just
    in
    case itemsAndErrors of
        [] ->
            model

        ( a, err ) :: xs ->
            if Set.member a model.loading then
                -- It was actually loading
                fail xs
                    { model
                        | loading = Set.remove a model.loading
                        , failed = Set.insert a model.failed
                        , errors = Dict.insert a err model.errors
                        , failCounts = Dict.update a countBumper model.failCounts
                    }

            else
                fail xs model


{-| Requeue items that have not yet exceeded the maxRetries
-}
requeueAllFailed : RemoteSet err comparable -> RemoteSet err comparable
requeueAllFailed model =
    let
        mapper : ( comparable, Int ) -> Maybe comparable
        mapper ( a, failCount ) =
            if failCount <= model.maxRetries then
                Just a

            else
                Nothing

        validForRequeue : Set comparable
        validForRequeue =
            Dict.toList model.failCounts
                |> List.filterMap mapper
                |> Set.fromList
    in
    { model
        | queued = Set.union validForRequeue model.queued
        , failed = Set.diff model.failed validForRequeue
    }
