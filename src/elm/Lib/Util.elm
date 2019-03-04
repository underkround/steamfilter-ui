module Lib.Util exposing
    ( allPredicates
    , anyPredicates
    , flip
    , httpErrorTostring
    , onKeyCodeDown
    , onKeyCodeUp
    )

import Html as H exposing (Html)
import Html.Events as Ev
import Http
import Json.Decode as JD


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a


allPredicates : List (a -> Bool) -> a -> Bool
allPredicates predicates x =
    case predicates of
        [] ->
            True

        f :: fs ->
            if f x then
                allPredicates fs x

            else
                False


anyPredicates : List (a -> Bool) -> a -> Bool
anyPredicates predicates x =
    case predicates of
        [] ->
            True

        f :: fs ->
            if f x then
                True

            else
                allPredicates fs x


onKeyCodeDown : (Int -> msg) -> H.Attribute msg
onKeyCodeDown tagger =
    Ev.on "keydown" (JD.map tagger Ev.keyCode)


onKeyCodeUp : (Int -> msg) -> H.Attribute msg
onKeyCodeUp tagger =
    Ev.on "keyup" (JD.map tagger Ev.keyCode)


httpErrorTostring : Http.Error -> String
httpErrorTostring err =
    case err of
        Http.BadUrl msg ->
            "HTTP BadUrl, no valid URL provided"

        Http.Timeout ->
            "HTTP Timeout, service took too long to response"

        Http.NetworkError ->
            "HTTP NetworkError, there might be a problem with your network"

        Http.BadStatus code ->
            "HTTP BadStatus (code " ++ String.fromInt code ++ "), service indicates an error"

        Http.BadBody details ->
            "HTTP BadBody, service responded with invalid data (problem: " ++ details ++ ")"
