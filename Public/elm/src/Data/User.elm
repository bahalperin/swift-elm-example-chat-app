module Data.User
    exposing
        ( Username
        , usernameToString
        , usernameFromString
        , decoder
        )

import Json.Decode exposing (Decoder)


type Username
    = Username String


usernameToString : Username -> String
usernameToString (Username username) =
    username


usernameFromString : String -> Username
usernameFromString string =
    Username string


decoder : Decoder String
decoder =
    Json.Decode.field "username" Json.Decode.string
