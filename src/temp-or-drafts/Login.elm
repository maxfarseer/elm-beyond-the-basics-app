module Login exposing (Model, Msg(..), initModel, login, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type alias Model =
    { username : String
    , password : String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    }


type Msg
    = UsernameInput String
    | PasswordInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            { model | username = username }

        PasswordInput password ->
            { model | password = password }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h3 [] [ Html.text "Login Page... So far" ]
        , Html.form []
            [ Html.input
                [ Attr.type_ "text"
                , Events.onInput UsernameInput
                , Attr.placeholder "username"
                ]
                []
            , Html.input
                [ Attr.type_ "password"
                , Events.onInput PasswordInput
                , Attr.placeholder "password"
                ]
                []
            , Html.input [ Attr.type_ "submit" ]
                [ Html.text "Login" ]
            ]
        ]


login : Program () Model Msg
login =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }
