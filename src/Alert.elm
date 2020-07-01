module Alert exposing (Alert(..))


type Alert
    = Warning String
    | Danger String
    | Positive String
