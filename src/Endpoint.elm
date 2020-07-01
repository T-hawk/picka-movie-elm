module Endpoint exposing (..)

-- USER


user : Int -> String
user id =
    "/api/user/" ++ String.fromInt id


login =
    "/api/login"



-- MOVIE


releases =
    "/api/releases"


search : String -> String
search query =
    "/api/search?query=" ++ query



-- LIBRARY


libraryAdd : Int -> String
libraryAdd id =
    "/api/library/" ++ String.fromInt id ++ "/add"


libraryRemove : Int -> String
libraryRemove id =
    "/api/library/" ++ String.fromInt id ++ "/remove"


libraryMovies : Int -> String
libraryMovies id =
    "/api/library/" ++ String.fromInt id ++ "/movies"



-- SESSION


createSession =
    "/api/session/create"


vote : Int -> String
vote id =
    "/api/session/" ++ String.fromInt id ++ "/vote"


sessionMovies : Int -> String
sessionMovies id =
    "/api/session/" ++ String.fromInt id ++ "/movies"


stopSession : Int -> String
stopSession id =
    "/api/session/" ++ String.fromInt id ++ "/stop"


results : Int -> String
results id =
    "/api/results/" ++ String.fromInt id
