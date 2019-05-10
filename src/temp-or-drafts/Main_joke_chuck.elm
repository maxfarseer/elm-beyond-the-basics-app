module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode exposing (Decoder, at, field, int, list, map3, string)


type alias JokeRecord =
    { id : Int
    , joke : String
    , categories : List String
    }


type alias Model =
    JokeRecord


initModel : Model
initModel =
    { joke = "fetching joke..."
    , id = 0
    , categories = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, randomJoke )


jokeDecoder : Decoder JokeRecord
jokeDecoder =
    -- field "value" (field "joke" string)
    map3 JokeRecord
        (field "id" int)
        (field "joke" string)
        (field "categories" (list string))
        |> at [ "value" ]


randomJoke : Cmd Msg
randomJoke =
    Http.get
        { url = "http://api.icndb.com/jokes/random"
        , expect = Http.expectJson Joke jokeDecoder
        }


type Msg
    = Joke (Result Http.Error JokeRecord)
    | NewJoke


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Joke (Ok joke) ->
            ( joke, Cmd.none )

        Joke (Err err) ->
            ( { model | joke = Debug.toString err }, Cmd.none )

        NewJoke ->
            ( initModel, randomJoke )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


formatJoke : JokeRecord -> Html Msg
formatJoke record =
    Html.div []
        [ Html.p [] [ Html.text <| String.fromInt record.id ++ " | " ++ record.joke ]
        , Html.p [] [ Html.text ("Categories: " ++ String.join ", " record.categories) ]
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "playground" ]
        [ Html.h1 []
            [ Html.text "playground" ]
        , Html.div []
            [ formatJoke model ]
        , Html.div []
            [ Html.button
                [ Events.onClick NewJoke ]
                [ Html.text "New joke" ]
            ]
        ]
