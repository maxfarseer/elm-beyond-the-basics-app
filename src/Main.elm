port module Main exposing (Model, Msg(..), Page(..), init, main, pageHeader, subscriptions, update, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.LeaderBoard as LBPage
import Pages.Login as LoginPage
import Pages.Runner as RunnerPage
import Routes exposing (Route)
import Shared exposing (Flags)
import Url exposing (Url)



-- model


type alias Model =
    { flags : Flags
    , navKey : Key
    , route : Route
    , page : Page
    }


type Page
    = NotFound
    | LoginPage LoginPage.Model
    | LBPage LBPage.Model
    | RunnerPage RunnerPage.Model


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginPageMsg LoginPage.Msg
    | LBPageMsg LBPage.Msg
    | RunnerPageMsg RunnerPage.Msg


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { flags = flags
            , navKey = navKey
            , route = Routes.parseUrl url
            , page = NotFound
            }
    in
    ( model, Cmd.none )
        |> loadCurrentPage


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( page, newCmd ) =
            case model.route of
                Routes.LoginRoute ->
                    let
                        ( pageModel, pageCmd ) =
                            LoginPage.init ()
                    in
                    ( LoginPage pageModel, Cmd.map LoginPageMsg pageCmd )

                Routes.LeaderBoardRoute ->
                    let
                        ( pageModel, pageCmd ) =
                            LBPage.init ()
                    in
                    ( LBPage pageModel, Cmd.map LBPageMsg pageCmd )

                Routes.RunnerRoute ->
                    let
                        ( pageModel, pageCmd ) =
                            RunnerPage.init ()
                    in
                    ( RunnerPage pageModel, Cmd.map RunnerPageMsg pageCmd )

                Routes.NotFoundRoute ->
                    ( NotFound, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, newCmd ] )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Update" ( msg, model.page )
    in
    case ( msg, model.page ) of
        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( OnUrlChange url, _ ) ->
            let
                newRoute =
                    Routes.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> loadCurrentPage

        ( LBPageMsg subMsg, LBPage pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    LBPage.update subMsg pageModel
            in
            ( { model | page = LBPage newPageModel }
            , Cmd.map LBPageMsg newCmd
            )

        ( LBPageMsg subMsg, _ ) ->
            ( model, Cmd.none )

        ( LoginPageMsg subMsg, LoginPage pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    LoginPage.update subMsg pageModel
            in
            ( { model | page = LoginPage newPageModel }
            , Cmd.map LoginPageMsg newCmd
            )

        ( LoginPageMsg subMsg, _ ) ->
            ( model, Cmd.none )

        -- case ( msg, model.page ) of
        ( RunnerPageMsg subMsg, RunnerPage pageModel ) ->
            let
                ( newPageModel, newCmd ) =
                    RunnerPage.update subMsg pageModel
            in
            ( { model | page = RunnerPage newPageModel }, Cmd.map RunnerPageMsg newCmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Leader board App"
    , body = [ currentPage model ]
    }


currentPage : Model -> Html Msg
currentPage model =
    let
        page =
            case model.page of
                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]

                LoginPage pageModel ->
                    div []
                        [ LoginPage.view pageModel
                            |> Html.map LoginPageMsg
                        ]

                LBPage pageModel ->
                    div []
                        [ LBPage.view pageModel
                            |> Html.map LBPageMsg
                        ]

                RunnerPage pageModel ->
                    div []
                        [ RunnerPage.view pageModel
                            |> Html.map RunnerPageMsg
                        ]
    in
    div []
        [ pageHeader model
        , page
        ]


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ href "/" ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ href "/" ] [ text "Link" ]
                ]
            ]
        , ul []
            [ li []
                [ a [ href Routes.loginPath ] [ text "Login" ]
                , a [ href Routes.addPath ] [ text "Add runner" ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
