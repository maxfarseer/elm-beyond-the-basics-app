port module Main exposing (Model, Msg(..), Page(..), init, initModel, main, pageHeader, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- model


type alias Model =
    { page : Page
    }


type Page
    = NotFound


initModel : Model
initModel =
    { page = NotFound }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
    div []
        [ pageHeader model
        , page
        ]


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ href "#/" ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ href "#" ] [ text "Link" ]
                ]
            ]
        , ul []
            [ li []
                [ a [ href "#" ] [ text "Login" ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
