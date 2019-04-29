module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke"


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


type Msg
    = Joke String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Joke str ->
            ( str, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "playground" ]
        [ Html.h1 []
            [ Html.text "playground" ]
        , Html.div []
            [ Html.text model ]
        ]
