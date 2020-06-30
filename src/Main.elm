port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
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
            { page = Home, movies = None, url = url, key = key, session = Session (User "" userId) NoSearch NoForm NoAlert }
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
                        FindForm value ->
                            value

                        _ ->
                            []

                cmd =
                    case movies of
                        [] ->
                            Cmd.none

                        _ ->
                            Http.post
                                { url = "/session/create"
                                , body = Http.jsonBody (Movie.encodeMovies movies)
                                , expect = Http.expectString SessionStarted
                                }
            in
            ( model, cmd )

        SessionStarted response ->
            let
                return =
                    case response of
                        Ok value ->
                            case value of
                                "OK" ->
                                    ( { model | page = Find }, "/find" )

                                "ACTIVE FINDER" ->
                                    ( { model | page = Find }, "/find" )

                                _ ->
                                    ( { model | page = Home }, "/#" )

                        Err _ ->
                            ( { model | page = Home }, "/#" )
            in
            ( Tuple.first return, Nav.pushUrl (Tuple.first return).key (Tuple.second return) )

        SessionEnded ->
            ( { model | page = Results }
            , Cmd.batch
                [ Http.post
                    { url = "/session/end"
                    , body = Http.emptyBody
                    , expect = Http.expectJson RecievedBestMovie Movie.decodeMovies
                    }
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
                    Session model.session.user (SearchOf string) model.session.form NoAlert
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
            ( model, Http.post { url = "/search", body = Http.stringBody "search" search, expect = Http.expectJson RecievedMovies Movie.decodeMovies } )

        SubmittedForm ->
            let
                user =
                    case model.session.form of
                        UserForm value ->
                            value

                        _ ->
                            User.defaultFormUser

                encodedUser =
                    case model.session.form of
                        UserForm value ->
                            User.encodeUser value

                        _ ->
                            User.encodeUser User.defaultFormUser
            in
            if user.password == user.passwordConf then
                ( { model | page = Home, session = Models.updateAlert model.session NoAlert }
                , Cmd.batch
                    [ Http.post
                        { url = "/api/login"
                        , body = Http.jsonBody encodedUser
                        , expect = Http.expectJson RecievedUser decodeUser
                        }
                    , Nav.pushUrl model.key <| "/"
                    ]
                )

            else
                let
                    oldSession =
                        model.session

                    session =
                        { oldSession | alert = Danger "Passwords do not match" }
                in
                ( { model | session = Models.updateAlert model.session (Danger "Passwords do not match") }, Cmd.none )

        Voted id ->
            let
                cmd =
                    Http.post
                        { url = "/vote"
                        , body = Http.jsonBody (encodeVote id model.session)
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }
            in
            ( model, cmd )

        AddedLibraryMovie movie ->
            let
                cmd =
                    Http.post
                        { url = "/library/add"
                        , body = Http.jsonBody (encodeVote movie.id model.session)
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }
            in
            ( model, cmd )

        RemovedLibraryMovie movie ->
            let
                cmd =
                    Http.post
                        { url = "/library/remove"
                        , body = Http.jsonBody (encodeVote movie.id model.session)
                        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
                        }
            in
            ( model, cmd )

        AddedFindMovie movie ->
            let
                movies =
                    case model.session.form of
                        FindForm value ->
                            value

                        _ ->
                            []

                appendMovie =
                    if List.member movie movies then
                        []

                    else
                        [ movie ]

                session =
                    Session model.session.user model.session.search (FindForm (List.append movies appendMovie)) NoAlert
            in
            ( { model | session = session }, Cmd.none )

        RemovedFindMovie movie ->
            let
                movies =
                    case model.session.form of
                        FindForm value ->
                            value

                        _ ->
                            []

                session =
                    Session model.session.user model.session.search (FindForm (remove movie movies)) NoAlert
            in
            ( { model | session = session }, Cmd.none )

        RecievedUser user ->
            let
                cmd =
                    case user of
                        Ok value ->
                            sendUserId (encodeUserId value.id)

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
        alert =
            Page.alertView model.session.alert
    in
    { title = "Picka Movie " ++ Page.titleOf model.page
    , body =
        [ Page.jumbotron model
        , Page.navbar model
        , alert
        , Page.content model
        ]
    }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


decodeFlags : Decode.Decoder Int
decodeFlags =
    Decode.field "id" Decode.int


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        ]


encodeUserId : Int -> Encode.Value
encodeUserId id =
    Encode.object
        [ ( "id", Encode.int id )
        ]


encodeVote : Int -> Session -> Encode.Value
encodeVote movieId session =
    Encode.object
        [ ( "movieId", Encode.int movieId )
        , ( "userId", Encode.int session.user.id )
        ]
