module Page exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Movie
import Msg exposing (..)
import Url
import User exposing (User)


jumbotron : Model -> Html msg
jumbotron model =
    let
        head =
            case model.page of
                Home ->
                    "Having Trouble Finding a Movie?"

                Find ->
                    "Vote for a Movie"

                NewFind ->
                    "Creating New Session"

                Library ->
                    "Your Library"

                WatchLater ->
                    "Movies to Watch Later"

                Login ->
                    "Login"

                Results ->
                    "Results"

        paragraph =
            case model.page of
                Home ->
                    "Worry no longer! Picka-movie has your back! This service will allow you to pick a movie very quickly!"

                _ ->
                    ""
    in
    div [ class "jumbotron text-center bg-primary text-white rounded-0 mb-0" ]
        [ h1 [] [ text head ]
        , p [] [ text paragraph ]
        ]


type alias NavLink =
    { text : String
    , url : String
    }


navbar : Model -> Html msg
navbar model =
    let
        links =
            case model.page of
                Home ->
                    [ NavLink "Library" "/library", NavLink "Find" "/find" ]

                Find ->
                    [ NavLink "Library" "/library" ]

                NewFind ->
                    [ NavLink "Library" "/library", NavLink "Find" "/find" ]

                Library ->
                    [ NavLink "Find" "/find" ]

                WatchLater ->
                    [ NavLink "Library" "/library", NavLink "Find" "/find" ]

                Login ->
                    [ NavLink "Library" "/library", NavLink "Find" "/find" ]

                Results ->
                    [ NavLink "Library" "/library", NavLink "Find" "/find" ]

        maybeUser =
            case model.session.user.name of
                "" ->
                    a [ href "/login" ] [ text "Sign in" ]

                _ ->
                    text model.session.user.name
    in
    nav [ class "navbar navbar-expand-sm alert alert-primary text-primary rounded-0 justify-content-between" ]
        [ ul [ class "navbar-nav" ]
            (List.append
                [ a [ class "navbar-brand", href "/#" ] [ text "Picka-Movie" ] ]
                (List.map navlink links)
            )
        , span [ class "navbar-text" ] [ maybeUser ]
        ]


titleOf : Page -> String
titleOf page =
    case page of
        Home ->
            ""

        Find ->
            " | Find"

        NewFind ->
            " | New Session"

        Library ->
            " | Library"

        WatchLater ->
            " | Watch Later"

        Login ->
            " | Login"

        Results ->
            " | Results"


content : Model -> Html Msg
content model =
    case model.page of
        Home ->
            homeView model

        Find ->
            findView model

        NewFind ->
            findFormView model

        Library ->
            libraryView model

        WatchLater ->
            laterView model

        Login ->
            loginView model

        Results ->
            resultsView model


homeView : Model -> Html msg
homeView model =
    let
        link =
            case model.session.user.name of
                "" ->
                    "/login"

                _ ->
                    "/newsession"
    in
    div [ class "m-4" ]
        [ div [ class "container shadow p-3 rounded" ]
            [ h1 [ class "text-center font-weight-bold" ] [ text "Features" ]
            , br [] []
            , br [] []
            , div [ class "row d-flex" ]
                [ div [ class "col-sm-4" ]
                    [ h1 [ class "text-center" ] [ text "Your Library" ]
                    , p [] [ text "Your library contains all the movies that you own! You may add or remove these movies from your list at any time!" ]
                    ]
                , div [ class "col-sm-4" ]
                    [ h1 [ class "text-center" ] [ text "Start a Movie Finding Session" ]
                    , p [] [ text "To start a movie finding session just click here! Once you start one, I will give you a link to share with you family! This session will last 30 min. If you are the creator of it, you may stop it at any time." ]
                    ]
                , div [ class "col-sm-4" ]
                    [ h1 [ class "text-center" ] [ text "Add to your Watch Later" ]
                    , p [] [ text "If you find a movie that you think you might enjoy, just add it to your watch later!" ]
                    ]
                ]
            ]
        , br [] []
        , div [ class "d-flex" ]
            [ div [ class "flex-fill text-center" ] [ a [ class "p-3 btn btn-outline-success", href "/library" ] [ text "Go to your Library" ] ]
            , div [ class "flex-fill text-center" ] [ a [ class "p-3 btn btn-outline-success", href link ] [ text "Start Movie Finding Session" ] ]
            , div [ class "flex-fill text-center" ] [ a [ class "p-3 btn btn-outline-secondary disabled", href "/later" ] [ text "See Your Watch Later List" ] ]
            ]
        ]


findView : Model -> Html Msg
findView model =
    let
        pageContent =
            case model.movies of
                Loading ->
                    h3 [] [ text "Loading" ]

                Success movies ->
                    case movies of
                        [] ->
                            h3 [] [ text "It appears that you haven't yet started a movie session. ", a [ href "/newsession" ] [ text "Click here to start!" ] ]

                        _ ->
                            div [ class "p-5" ]
                                [ searchBar model.session.search
                                , button [ class "btn btn-danger center", onClick SessionEnded ] [ text "End Session" ]
                                , div [ class "container row" ] (List.map (\movie -> Movie.findView movie) movies)
                                ]

                _ ->
                    h3 [] [ text "Failed to load movies for some reason..." ]
    in
    div [ class "m-4" ]
        [ pageContent
        ]


loginView : Model -> Html Msg
loginView model =
    let
        user =
            case model.session.form of
                UserForm value ->
                    value

                _ ->
                    User.FormUser "" "" "" "" -1
    in
    div [ class "container" ]
        [ input [ placeholder "Name", value user.name, onInput (FormChanged Name), class "form-control" ] []
        , br [] []
        , input [ placeholder "Email", value user.email, onInput (FormChanged Email), class "form-control", type_ "email" ] []
        , br [] []
        , input [ placeholder "Password", value user.password, onInput (FormChanged Password), class "form-control", type_ "password" ] []
        , br [] []
        , input [ placeholder "Password Confirmation", value user.passwordConf, onInput (FormChanged PasswordConf), class "form-control", type_ "password" ] []
        , br [] []
        , button [ class "btn btn-success", onClick SubmittedForm ] [ text "Login" ]
        ]


findFormView : Model -> Html Msg
findFormView model =
    let
        suggestions =
            case model.movies of
                Loading ->
                    h3 [] [ text "Loading" ]

                Success movies ->
                    div [] (List.map (\movie -> Movie.findFormView movie) movies)

                _ ->
                    h3 [] [ text "Failed to load movies for some reason..." ]

        currentMovies =
            case model.session.form of
                FindForm movies ->
                    div []
                        (List.map
                            (\movie ->
                                p []
                                    [ text movie.title
                                    , button [ class "btn btn-danger ml-2", onClick (RemovedFindMovie movie) ] [ text "Remove" ]
                                    ]
                            )
                            movies
                        )

                _ ->
                    div [] []
    in
    if model.session.user.name == "" then
        div [ class "m-4" ] [ h3 [] [ text "Oops! It appears you haven't signed in yet... ", a [ href "/login" ] [ text "Login here" ] ] ]

    else
        div [ class "m-4" ]
            [ h1 [] [ text "Select the movies that you would like to appear in the session..." ]
            , br [] []
            , button [ class "btn btn-success", onClick StartedSession ] [ text "Create Session" ]
            , searchBar model.session.search
            , div [ class "sticky-top bg-white float-right shadow rounded m-3 p-3" ]
                [ h3 [] [ text "Your Current Selections" ]
                , currentMovies
                ]
            , div []
                [ h2 [] [ text "Top Suggestions" ]
                , suggestions
                ]
            ]


libraryView : Model -> Html Msg
libraryView model =
    let
        pageContent =
            case model.movies of
                Loading ->
                    h3 [] [ text "Loading" ]

                Failure ->
                    h3 [] [ text "Failed to load movies for some reason..." ]

                Success movies ->
                    let
                        movieRender =
                            if List.length movies > 0 then
                                List.map (\movie -> Movie.libraryView movie) movies

                            else
                                [ h2 [] [ text "There are no movies in your library..." ] ]
                    in
                    div [ class "container row" ] (List.append [ searchBar model.session.search ] movieRender)

                _ ->
                    h3 [] [ text "Failed to load movies for some reason..." ]
    in
    div [ class "m-4" ]
        [ pageContent
        ]


laterView : Model -> Html Msg
laterView model =
    let
        pageContent =
            case model.movies of
                Loading ->
                    h3 [] [ text "Loading" ]

                Success movies ->
                    div [ class "container row" ] (List.map (\movie -> Movie.laterView movie) movies)

                _ ->
                    h3 [] [ text "Failed to load movies for some reason..." ]
    in
    div []
        [ pageContent
        , searchBar model.session.search
        ]


resultsView : Model -> Html Msg
resultsView model =
    let
        pageContent =
            case model.movies of
                Loading ->
                    h3 [] [ text "Loading" ]

                Success movies ->
                    div []
                        [ h3 [ class "text-center" ] [ text "It was a tie! See the winners down here!" ]
                        , div [ class "text-center" ] (List.map (\movie -> Movie.resultsView movie) movies)
                        ]

                Single movie ->
                    div []
                        [ h3 [ class "text-center" ] [ text ("You're watching " ++ movie.title ++ "!") ]
                        , Movie.resultsView movie
                        ]

                _ ->
                    h3 [] [ text "Failed to load movie..." ]
    in
    div [ class "m-4" ]
        [ pageContent ]


searchBar : Search -> Html Msg
searchBar search =
    let
        searchOf =
            case search of
                SearchOf value ->
                    value

                NoSearch ->
                    ""
    in
    div [ class "input-group mb-3" ]
        [ input [ class "form-control", placeholder "Search...", onInput SearchChanged ] []
        , div [ class "input-group-append" ] [ button [ class "btn btn-outline-primary", value searchOf, onClick SubmittedSearch ] [ text "Find" ] ]
        ]



-- Helpers


navlink : NavLink -> Html msg
navlink link =
    li [ class "nav-item" ] [ a [ href link.url, class "nav-link" ] [ text link.text ] ]


alertView : Alert -> Html msg
alertView alert =
    let
        divContent =
            case alert of
                Danger string ->
                    ( "alert alert-danger", string )

                Warning string ->
                    ( "alert alert-warning", string )

                Positive string ->
                    ( "alert alert-success", string )

                NoAlert ->
                    ( "", "" )
    in
    div [ class (Tuple.first divContent ++ " m-5") ] [ text (Tuple.second divContent) ]
