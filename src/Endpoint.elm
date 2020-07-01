module Endpoint exposing (..)

-- USER


user : Int -> String
user id =
    "/api/results/" ++ String.fromInt id


login =
    "/api/login"



-- MOVIE


releases =
    "/api/releases"


search : String -> String
search query =
    "/api/search?query=" ++ query



-- LIBRARY


libraryAdd =
    "/api/library/add"


libraryRemove =
    "/api/library/remove"


libraryMovies =
    "/api/library/movies"



-- SESSION


createSession =
    "/api/session/create"


vote =
    "/api/session/vote"


sessionMovies =
    "/api/session/movies"


stopSession =
    "/api/session/stop"


results =
    "/api/results/movies"
