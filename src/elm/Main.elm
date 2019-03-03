module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import GameGrid
import Html as H exposing (Html)
import Html.Attributes as At
import Html.Events as Ev
import Route exposing (Route)
import Url exposing (Url)
import Widget.Statics


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
        contentPage : Html Msg
        contentPage =
            case model.route of
                Nothing ->
                    H.text "404 lol"

                Just Route.About ->
                    H.text "what"

                Just Route.GameGrid ->
                    GameGrid.view GameGridMsg model.gameGrid
    in
    { title = "Steam Filter"
    , body =
        [ Widget.Statics.header
        , contentPage
        , Widget.Statics.footer
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
