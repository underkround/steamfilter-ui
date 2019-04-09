module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Filters exposing (Filters)
import GameGrid
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Route exposing (Route)
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    { key : Navigation.Key
    , url : Url
    , route : Maybe Route
    , gameGrid : GameGrid.Model
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , route = Route.fromUrl url
      , gameGrid = GameGrid.init
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | LinkClick Browser.UrlRequest
    | UrlChange Url.Url
    | GameGridMsg GameGrid.Msg


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

        -- Submessages
        GameGridMsg subMsg ->
            GameGrid.update GameGridMsg subMsg model.gameGrid
                |> Tuple.mapFirst (\m -> { model | gameGrid = m })


view : Model -> Browser.Document Msg
view model =
    let
        headerView : Html msg
        headerView =
            H.header
                [ At.class "navbar navbar-dark bg-dark justify-content-between"
                ]
                [ H.a
                    [ At.class "navbar-brand"
                    , At.href "https://steamfilter.net"
                    ]
                    [ H.text "Steam Filter" ]
                , H.span [ At.class "navbar-text bg-danger rounded p-2" ]
                    [ H.text "WORK IN PROGRESS!" ]
                , H.ul [ At.class "navbar-nav" ]
                    [ H.li [ At.class "nav-item" ]
                        [ H.a
                            [ At.class "nav-link"
                            , At.href "https://github.com/underkround/steamfilter-ui"
                            ]
                            [ H.code [] [ H.text "src" ]
                            ]
                        ]
                    ]
                ]

        footerView : Html msg
        footerView =
            H.footer []
                [ H.text "Footer" ]

        contentPage : Html Msg
        contentPage =
            H.div [ At.class "container-fluid" ]
                [ case model.route of
                    Nothing ->
                        H.text "404 lol"

                    Just Route.About ->
                        H.text "what"

                    Just Route.GameGrid ->
                        GameGrid.view GameGridMsg model.gameGrid
                ]
    in
    { title = "Steam Filter"
    , body =
        [ headerView
        , contentPage
        , footerView
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
