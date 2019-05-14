module Pages.Login exposing (Model, Msg(..), errorPanel, init, initModel, loginForm, subscriptions, update, view)

import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field)
import Json.Encode as JE
import Url



-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    , navKey : Key
    }


initModel : Key -> Model
initModel navKey =
    { username = ""
    , password = ""
    , error = Nothing
    , navKey = navKey
    }


init : Key -> ( Model, Cmd Msg )
init key =
    ( initModel key, Cmd.none )


tokenDecoder : Decoder String
tokenDecoder =
    field "token" JD.string



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String
    | LoginResponse (Result Http.Error String)


url : String
url =
    "http://localhost:5000/authenticate"


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                rawBody =
                    JE.object
                        [ ( "username", JE.string model.username )
                        , ( "password", JE.string model.password )
                        ]

                body =
                    Http.jsonBody rawBody

                cmd =
                    Http.request
                        { method = "POST"
                        , headers = []
                        , url = url
                        , body = body
                        , expect = Http.expectJson LoginResponse tokenDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
            in
            ( model, cmd, Nothing )

        LoginResponse (Ok token) ->
            ( initModel model.navKey, pushUrl model.navKey "/", Just token )

        LoginResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err

                errMsg =
                    case err of
                        Http.BadStatus resp ->
                            case resp of
                                401 ->
                                    "401"

                                _ ->
                                    "Error with unhandled Http.BadStatus"

                        _ ->
                            "Login error!"
            in
            ( { model | error = Just errMsg }, Cmd.none, Nothing )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Nothing )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ class "add-runner", onSubmit Submit ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div []
                [ label [] [ text "User Name" ]
                , input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div []
                [ label [] []
                , button [ type_ "submit" ] [ text "Login" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
