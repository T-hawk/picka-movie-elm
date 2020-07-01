module Api exposing (..)

import Endpoint
import Http
import Json.Encode as Encode
import Models exposing (Form(..), Movie, Session)
import Movie
import Msg exposing (Msg(..))
import Url
import User exposing (User)


createSession : List Movie -> Cmd Msg
createSession movies =
    Http.post
        { url = Endpoint.createSession
        , body = Http.jsonBody (Movie.encodeMovies movies)
        , expect = Http.expectString SessionStarted
        }


vote : Int -> Session -> Cmd Msg
vote id session =
    Http.post
        { url = Endpoint.vote
        , body = Http.jsonBody (encodeVote id session)
        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
        }


stopSession : Cmd Msg
stopSession =
    Http.post
        { url = Endpoint.stopSession
        , body = Http.emptyBody
        , expect = Http.expectJson RecievedBestMovie Movie.decodeMovies
        }


search : String -> Cmd Msg
search query =
    let
        formattedQuery =
            Url.percentEncode query
    in
    Http.get
        { url = Endpoint.search formattedQuery
        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
        }


login : Form -> Cmd Msg
login form =
    let
        encodedUser =
            case form of
                UserForm value ->
                    User.encodeUser value

                _ ->
                    User.encodeUser User.defaultFormUser
    in
    Http.post
        { url = Endpoint.login
        , body = Http.jsonBody encodedUser
        , expect = Http.expectJson RecievedUser User.decodeUser
        }


libraryAdd : Int -> Session -> Cmd Msg
libraryAdd id session =
    Http.post
        { url = Endpoint.libraryAdd
        , body = Http.jsonBody (encodeVote id session)
        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
        }


libraryRemove : Int -> Session -> Cmd Msg
libraryRemove id session =
    Http.post
        { url = Endpoint.libraryRemove
        , body = Http.jsonBody (encodeVote id session)
        , expect = Http.expectJson RecievedMovies Movie.decodeMovies
        }



-- HELPERS


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
