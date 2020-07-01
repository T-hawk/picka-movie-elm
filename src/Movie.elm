module Movie exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decoder exposing (bool, field, int, map4, map5, string)
import Json.Encode as Encoder
import Models exposing (Movie)
import Msg exposing (..)


libraryView : Movie -> Html Msg
libraryView movie =
    let
        addButton =
            if movie.inLibrary then
                button [ class "btn btn-danger w-50 mt-2 font-weight-bold", onClick (RemovedLibraryMovie movie) ] [ text "Remove" ]

            else
                button [ class "btn btn-success w-50 mt-2 font-weight-bold", onClick (AddedLibraryMovie movie) ] [ text "Add" ]
    in
    div [ class "p-3 m-3 shadow text-center rounded", style "width" "20rem" ]
        [ h3 [] [ text movie.title ]
        , img [ src ("http://image.tmdb.org/t/p/w185//" ++ movie.pic) ] []
        , addButton
        ]


laterView : Movie -> Html Msg
laterView movie =
    div [ class "p-3 m-3 shadow text-center rounded", style "width" "20rem" ]
        [ h3 [] [ text movie.title ]
        , img [ src ("http://image.tmdb.org/t/p/w185//" ++ movie.pic) ] []
        , a [ class "btn btn-success w-50 mt-2 font-weight-bold", href "/#" ] [ text "Watched it!" ]
        ]


movieSessionView : Movie -> Html Msg
movieSessionView movie =
    div [ class "p-3 m-3 shadow text-center rounded", style "width" "20rem" ]
        [ h3 [] [ text movie.title ]
        , img [ src ("http://image.tmdb.org/t/p/w185//" ++ movie.pic) ] []
        , p [] [ text (String.fromInt movie.votes) ]
        , button [ class "btn btn-success w-50 mt-2 font-weight-bold", onClick (Voted movie.id) ] [ text "Vote" ]
        ]


movieSessionFormView : Movie -> Html Msg
movieSessionFormView movie =
    div [ class "p-3 m-3 shadow text-center rounded", style "width" "20rem" ]
        [ h3 [] [ text movie.title ]
        , img [ src ("http://image.tmdb.org/t/p/w185//" ++ movie.pic) ] []
        , button [ class "btn btn-primary w-50 mt-2 font-weight-bold", onClick (AddedMovieSessionMovie movie) ] [ text "Add" ]
        ]


resultsView : Movie -> Html msg
resultsView movie =
    div [ class "p-3 m-3 shadow text-center rounded", style "width" "20rem" ]
        [ h3 [] [ text movie.title ]
        , img [ src ("http://image.tmdb.org/t/p/w185//" ++ movie.pic) ] []
        ]


decodeMovie : Decoder.Decoder Movie
decodeMovie =
    map5 Movie
        (field "title" string)
        (field "poster_path" string)
        (field "votes" int)
        (field "id" int)
        (field "in_library" bool)


decodeMovies : Decoder.Decoder (List Movie)
decodeMovies =
    Decoder.list decodeMovie


encodeMovies : List Movie -> Encoder.Value
encodeMovies movies =
    Encoder.object
        [ ( "Movies", Encoder.list Encoder.int <| List.map (\movie -> movie.id) movies ) ]
