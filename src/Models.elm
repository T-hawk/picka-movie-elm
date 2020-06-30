module Models exposing (..)

import Browser.Navigation as Nav
import Http
import Url
import User exposing (FormUser, User)


type alias Model =
    { page : Page
    , session : Session
    , movies : Movies
    , key : Nav.Key
    , url : Url.Url
    }


type Page
    = Home
    | Find
    | Library
    | WatchLater
    | Login
    | NewFind
    | Results


type Form
    = NoForm
    | UserForm FormUser
    | FindForm (List Movie)


type Search
    = SearchOf String
    | NoSearch


type alias Session =
    { user : User
    , search : Search
    , form : Form
    , alert : Alert
    }


type alias Movie =
    { title : String
    , pic : String
    , votes : Int
    , id : Int
    , inLibrary : Bool
    }


type Movies
    = Loading
    | Failure
    | Success (List Movie)
    | Single Movie
    | None


type Field
    = Name
    | Email
    | Password
    | PasswordConf


type Alert
    = Warning String
    | Danger String
    | Positive String
    | NoAlert


updateAlert : Session -> Alert -> Session
updateAlert session alert =
    { session | alert = alert }
