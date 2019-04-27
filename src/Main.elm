module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import LeaderBoard
import Login


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    }


initModel : Model
initModel =
    { page = LeaderBoardPage
    , leaderBoard = LeaderBoard.initModel
    , login = Login.initModel
    }


type Page
    = LeaderBoardPage
    | LoginPage


type Msg
    = ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        LeaderBoardMsg lbMsg ->
            ( { model | leaderBoard = LeaderBoard.update lbMsg model.leaderBoard }, Cmd.none )

        LoginMsg loginMsg ->
            ( { model | login = Login.update loginMsg model.login }, Cmd.none )


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
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMsg
                        (LeaderBoard.view model.leaderBoard)

                LoginPage ->
                    Html.map LoginMsg
                        (Login.view model.login)
    in
    Html.div []
        [ Html.div []
            [ Html.a
                [ Attr.href "#"
                , Events.onClick (ChangePage LeaderBoardPage)
                ]
                [ Html.text "LeaderBoard" ]
            , Html.span [] [ Html.text " | " ]
            , Html.a
                [ Attr.href "#"
                , Events.onClick (ChangePage LoginPage)
                ]
                [ Html.text "Login" ]
            ]
        , Html.hr [] []
        , page
        ]
