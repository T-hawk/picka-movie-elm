module Router exposing (..)

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
        , map Find (s "find")
        , map NewFind (s "newsession")
        , map Library (s "library")
        , map WatchLater (s "later")
        , map Login (s "login")
        , map Results (s "results")
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
                Login ->
                    { model | page = page, session = session, movies = None }

                Find ->
                    { model | page = page, session = session, movies = None }

                NewFind ->
                    { model | page = page, session = session, movies = Loading }

                Home ->
                    { model | page = page, session = session, movies = None }

                Library ->
                    { model | page = page, session = session, movies = Loading }

                WatchLater ->
                    { model | page = page, session = session, movies = None }

                Results ->
                    { model | page = page, session = session, movies = Loading }

        cmd =
            case page of
                Home ->
                    Cmd.none

                Login ->
                    Cmd.none

                Find ->
                    Http.get
                        { url = "/api/session/movies"
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }

                NewFind ->
                    Http.get
                        { url = "/releases"
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }

                Library ->
                    Http.get
                        { url = "/api/library/movies"
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }

                WatchLater ->
                    Cmd.none

                Results ->
                    Http.get
                        { url = "/get/results"
                        , expect = Http.expectJson RecievedBestMovie Movie.decodeMovies
                        }
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
