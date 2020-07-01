port module Main exposing (..)

import Alert exposing (Alert(..))
import Api
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (remove)
import Models exposing (..)
import Movie
import Msg exposing (..)
import Page
import PortFunnel.LocalStorage as LocalStorage
import Router exposing (updateView)
import Url
import User exposing (User, decodeUser)


port sendUserId : Encode.Value -> Cmd msg


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        userId =
            case Decode.decodeValue decodeFlags flags of
                Ok int ->
                    int

                Err _ ->
                    -1

        model =
            { page = Home, movies = None, url = url, key = key, session = Session (User "" userId) NoSearch NoForm [] }
    in
    updateView model.url model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            updateView url model

        StartedSession ->
            let
                movies =
                    case model.session.form of
                        MovieSessionForm value ->
                            value

                        _ ->
                            []

                cmd =
                    case movies of
                        [] ->
                            Cmd.none

                        _ ->
                            Api.createSession movies
            in
            ( model, cmd )

        SessionStarted response ->
            let
                return =
                    case response of
                        Ok value ->
                            case value of
                                "OK" ->
                                    ( { model | page = MovieSession }, "/find" )

                                "ACTIVE FINDER" ->
                                    ( { model | page = MovieSession }, "/find" )

                                _ ->
                                    ( { model | page = Home }, "/#" )

                        Err _ ->
                            ( { model | page = Home }, "/#" )
            in
            ( Tuple.first return, Nav.pushUrl (Tuple.first return).key (Tuple.second return) )

        SessionEnded ->
            ( { model | page = Results }
            , Cmd.batch
                [ Api.stopSession
                , Nav.pushUrl model.key "/results"
                ]
            )

        RecievedBestMovie movies ->
            let
                movie =
                    case movies of
                        Ok value ->
                            if List.length value == 1 then
                                case List.head value of
                                    Just x ->
                                        Single x

                                    Nothing ->
                                        Failure

                            else
                                Success value

                        Err _ ->
                            None
            in
            ( { model | movies = movie }, Cmd.none )

        FormChanged form value ->
            let
                oldSession =
                    model.session

                session =
                    case model.session.form of
                        UserForm user ->
                            case form of
                                Name ->
                                    { oldSession | form = UserForm { user | name = value } }

                                Email ->
                                    { oldSession | form = UserForm { user | email = value } }

                                Password ->
                                    { oldSession | form = UserForm { user | password = value } }

                                PasswordConf ->
                                    { oldSession | form = UserForm { user | passwordConf = value } }

                        _ ->
                            model.session
            in
            ( { model | session = session }, Cmd.none )

        SearchChanged string ->
            let
                session =
                    Session model.session.user (SearchOf string) model.session.form []
            in
            ( { model | session = session }, Cmd.none )

        SubmittedSearch ->
            let
                search =
                    case model.session.search of
                        NoSearch ->
                            ""

                        SearchOf value ->
                            value
            in
            ( model, Api.search search )

        SubmittedForm ->
            let
                alerts =
                    User.validUser
                        (case model.session.form of
                            UserForm value ->
                                value

                            _ ->
                                User.defaultFormUser
                        )

                _ =
                    Debug.log "alerts" alerts
            in
            if List.isEmpty alerts then
                ( { model | page = Home, session = Models.updateAlerts model.session [] }
                , Cmd.batch
                    [ Api.login model.session.form
                    , Nav.pushUrl model.key <| "/"
                    ]
                )

            else
                let
                    oldSession =
                        model.session

                    session =
                        { oldSession | alerts = alerts }
                in
                ( { model | session = session }, Cmd.none )

        Voted id ->
            ( model, Api.vote id model.session )

        AddedLibraryMovie movie ->
            ( model, Api.libraryAdd movie.id model.session )

        RemovedLibraryMovie movie ->
            ( model, Api.libraryRemove movie.id model.session )

        AddedMovieSessionMovie movie ->
            let
                movies =
                    case model.session.form of
                        MovieSessionForm value ->
                            value

                        _ ->
                            []

                appendMovie =
                    if List.member movie movies then
                        []

                    else
                        [ movie ]

                session =
                    Session model.session.user model.session.search (MovieSessionForm (List.append movies appendMovie)) []
            in
            ( { model | session = session }, Cmd.none )

        RemovedMovieSessionMovie movie ->
            let
                movies =
                    case model.session.form of
                        MovieSessionForm value ->
                            value

                        _ ->
                            []

                session =
                    Session model.session.user model.session.search (MovieSessionForm (remove movie movies)) []
            in
            ( { model | session = session }, Cmd.none )

        RecievedUser user ->
            let
                cmd =
                    case user of
                        Ok value ->
                            sendUserId (Api.encodeUserId value.id)

                        Err _ ->
                            Cmd.none

                maybeUser =
                    case user of
                        Ok value ->
                            value

                        Err _ ->
                            model.session.user

                oldSession =
                    model.session

                session =
                    { oldSession | user = maybeUser }

                _ =
                    Debug.log "user" user
            in
            ( { model | session = session }, cmd )

        RecievedUserName response ->
            let
                oldUser =
                    model.session.user

                newUser =
                    case response of
                        Ok value ->
                            case value of
                                "ERROR" ->
                                    User "" -1

                                _ ->
                                    { oldUser | name = value }

                        Err _ ->
                            User "" -1

                oldSession =
                    model.session

                newSession =
                    { oldSession | user = newUser }
            in
            ( { model | session = newSession }, Cmd.none )

        RecievedMovies movies ->
            let
                _ =
                    Debug.log "Movies" movies
            in
            case movies of
                Ok value ->
                    ( { model | movies = Success value }, Cmd.none )

                Err _ ->
                    ( { model | movies = Failure }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        alerts =
            Page.alertsView model.session.alerts
    in
    { title = "Picka Movie " ++ Page.titleOf model.page
    , body =
        [ Page.jumbotron model
        , Page.navbar model
        , alerts
        , Page.content model
        ]
    }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


decodeFlags : Decode.Decoder Int
decodeFlags =
    Decode.field "id" Decode.int
