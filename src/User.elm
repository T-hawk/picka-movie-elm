module User exposing (..)

import Alert exposing (Alert(..))
import Json.Decode as Decode exposing (field, int, list, map2, nullable, string)
import Json.Encode as Encode
import Validate exposing (isValidEmail)


type alias User =
    { name : String
    , id : Int
    }


type alias FormUser =
    { name : String
    , email : String
    , password : String
    , passwordConf : String
    , id : Int
    }


encodeUser : FormUser -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "email", Encode.string user.email )
        , ( "password", Encode.string user.password )
        ]


encodeId : Int -> Encode.Value
encodeId id =
    Encode.object
        [ ( "userId", Encode.int id ) ]


decodeUser : Decode.Decoder User
decodeUser =
    map2 User
        (field "name" string)
        (field "id" int)


defaultFormUser : FormUser
defaultFormUser =
    FormUser "" "" "" "" -1


validUser : FormUser -> List Alert
validUser user =
    List.filter isNoAlert <|
        List.map validateAttribute
            [ ( user.name /= "", "Name field must be filled" )
            , ( user.email /= "", "Email field must be filled" )
            , ( isValidEmail user.email, "Not a valid email" )
            , ( user.password /= "", "Password field must be filled" )
            , ( user.password == user.passwordConf, "Passwords do not match" )
            , ( String.length user.password >= 8, "Password must be longer than 8 characters" )
            ]


validateAttribute : ( Bool, String ) -> Alert
validateAttribute ( bool, error ) =
    if bool then
        Positive ""

    else
        Danger error


isNoAlert : Alert -> Bool
isNoAlert alert =
    case alert of
        Danger _ ->
            True

        _ ->
            False
