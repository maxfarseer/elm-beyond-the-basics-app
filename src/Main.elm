module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode exposing (Decoder, field, string)


type alias JokeRecord =
    { value : String
    , id : Int
    , categories : List String
    }


type alias Model =
    String


initModel : Model
initModel =
    "Finding a joke..."


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, randomJoke )


jokeDecoder : Decoder String
jokeDecoder =
    field "value" (field "joke" string)


randomJoke : Cmd Msg
randomJoke =
    Http.get
        { url = "http://api.icndb.com/jokes/random"
        , expect = Http.expectJson Joke jokeDecoder
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
                [ Html.text "New joke" ]
            ]
        ]
