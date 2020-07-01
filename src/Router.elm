module Router exposing (..)

import Api
import Http
import Models exposing (..)
import Movie
import Msg exposing (..)
import Page
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, top)
import User


parseUrl : Parser.Parser (Page -> a) a
parseUrl =
    oneOf
        [ map Home top
        , map User (s "user" </> int)
        , map MovieSession (s "find" </> int)
        , map NewMovieSession (s "newsession")
        , map Library (s "library" </> int)
        , map Login (s "login")
        , map Results (s "results" </> int)
        ]


updateView : Url.Url -> Model -> ( Model, Cmd Msg )
updateView url model =
    let
        page =
            case decode url of
                Nothing ->
                    Home

                Just value ->
                    value

        oldSession =
            model.session

        session =
            case page of
                Login ->
                    { oldSession | form = UserForm User.defaultFormUser }

                _ ->
                    model.session

        returnModel =
            case page of
                NewMovieSession ->
                    { model | page = page, session = session, movies = Loading }

                Library _ ->
                    { model | page = page, session = session, movies = Loading }

                Results _ ->
                    { model | page = page, session = session, movies = Loading }

                _ ->
                    { model | page = page, session = session, movies = None }

        cmd =
            case page of
                MovieSession ->
                    Http.get
                        { url = "/api/session/movies"
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }

                User id ->
                    Api.user id

                Results id ->
                    Api.results id

                NewMovieSession ->
                    Api.releases

                Library _ ->
                    Api.libraryMovies

                _ ->
                    Cmd.none
    in
    ( returnModel
    , Cmd.batch
        [ cmd
        , Http.post
            { url = "/user"
            , body = Http.jsonBody (User.encodeId model.session.user.id)
            , expect = Http.expectString RecievedUserName
            }
        ]
    )


decode : Url.Url -> Maybe Page
decode url =
    Parser.parse parseUrl url
