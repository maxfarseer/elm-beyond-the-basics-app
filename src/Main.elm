module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke..."


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, randomJoke )


randomJoke : Cmd Msg
randomJoke =
    Http.get
        { url = "http://api.icndb.com/jokes/random"
        , expect = Http.expectString Joke
        }


type Msg
    = Joke (Result Http.Error String)
    | NewJoke


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Joke (Ok joke) ->
            ( joke, Cmd.none )

        Joke (Err err) ->
            ( Debug.toString err, Cmd.none )

        NewJoke ->
            ( "fetching joke ...", randomJoke )


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
        , Html.div []
            [ Html.button
                [ Events.onClick NewJoke ]
                [ Html.text "qq" ]
            ]
        ]
