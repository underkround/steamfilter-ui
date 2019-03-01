module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, button, div, text)
import Html.Events as Ev
import Url exposing (Url)


type alias Model =
    Int


type alias Flags =
    {}


type Msg
    = NoOp
    | LinkClick Browser.UrlRequest
    | Increment
    | Decrement


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( 0, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = always NoOp
        , onUrlRequest = LinkClick
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClick _ ->
            ( model, Cmd.none )

        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Steam Filter"
    , body =
        [ button [ Ev.onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ Ev.onClick Increment ] [ text "+" ]
        ]
    }
