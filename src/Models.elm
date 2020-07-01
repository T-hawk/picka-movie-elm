module Models exposing (..)

import Alert exposing (Alert(..))
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
    , alerts : List Alert
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


updateAlerts : Session -> List Alert -> Session
updateAlerts session alerts =
    { session | alerts = alerts }
