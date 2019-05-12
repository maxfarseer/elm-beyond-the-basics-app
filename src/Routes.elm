module Routes exposing (Route(..), addPath, leaderBoardPath, loginPath, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = RunnerRoute
    | LoginRoute
    | LeaderBoardRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    let
        _ =
            Debug.log "Route" "RR"
    in
    oneOf
        [ map LeaderBoardRoute top
        , map RunnerRoute (s "add")
        , map LoginRoute (s "login")

        -- , map PlayerRoute (s "players" </> string)
        ]


parseUrl : Url -> Route
parseUrl url =
    let
        _ =
            Debug.log "parseUrl" (parse matchers url)
    in
    case parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


pathFor : Route -> String
pathFor route =
    case route of
        LeaderBoardRoute ->
            "/"

        RunnerRoute ->
            "/add"

        LoginRoute ->
            "/login"

        {- PlayerRoute id ->
           "/players/" ++ id
        -}
        NotFoundRoute ->
            "/not-found"


leaderBoardPath =
    pathFor LeaderBoardRoute


addPath =
    pathFor RunnerRoute


loginPath =
    pathFor LoginRoute
