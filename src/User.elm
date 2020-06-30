module User exposing (..)

import Json.Decode as Decode exposing (field, int, list, map2, nullable, string)
import Json.Encode as Encode


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
        , ( "email", Encode.string user.name )
        , ( "password", Encode.string user.name )
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
