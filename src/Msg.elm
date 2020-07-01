module Msg exposing (Msg(..))

import Browser
import Http
import Models exposing (Field, Movie)
import Url
import User exposing (User)


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | FormChanged Field String
    | SubmittedForm
    | StartedSession
    | SessionEnded
    | SessionStarted (Result Http.Error String)
    | SubmittedSearch
    | Voted Int
    | AddedLibraryMovie Movie
    | RemovedLibraryMovie Movie
    | AddedMovieSessionMovie Movie
    | RemovedMovieSessionMovie Movie
    | SearchChanged String
    | RecievedUser (Result Http.Error User)
    | RecievedBestMovie (Result Http.Error (List Movie))
    | RecievedUserName (Result Http.Error String)
    | RecievedMovies (Result Http.Error (List Movie))
