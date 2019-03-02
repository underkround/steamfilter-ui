module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Page.GameGrid
import Page.Home
import Route exposing (Route)
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    { key : Navigation.Key
    , url : Url
    , route : Maybe Route

    --, page : Maybe Page
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , route = Route.fromUrl url
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | LinkClick Browser.UrlRequest
    | UrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClick urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChange url ->
            ( { model
                | url = url
                , route = Route.fromUrl url
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    let
        link path =
            H.li [] [ H.a [ At.href path ] [ H.text path ] ]

        contentPage : Html Msg
        contentPage =
            case model.route of
                Nothing ->
                    H.text "404 lol"

                Just Route.Home ->
                    Page.Home.view

                Just Route.GameGrid ->
                    Page.GameGrid.view

        debugContent : Html Msg
        debugContent =
            H.pre
                [ At.class "debug" ]
                [ H.text <| "  Url: " ++ Url.toString model.url
                , H.text <|
                    "Route: "
                        ++ (model.route
                                |> Maybe.map Route.toString
                                |> Maybe.withDefault ""
                           )
                ]
    in
    { title = "Steam Filter"
    , body =
        [ debugContent
        , H.ul []
            [ link "#/"
            , link "#/show"
            ]
        , H.h2 [] [ H.text "Content" ]
        , contentPage
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClick
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
