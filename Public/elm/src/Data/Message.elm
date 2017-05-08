module Data.Message
    exposing
        ( Message
        , decoder
        , decode
        , encode
        )

import Date exposing (Date)
import Json.Decode exposing (Decoder)
import Json.Encode
import Data.User exposing (Username)


type alias Message =
    { username : Username
    , content : String
    , created : Maybe Date
    }


decode : String -> Result String Message
decode messageJson =
    Json.Decode.decodeString decoder messageJson


decoder : Decoder Message
decoder =
    Json.Decode.map3 Message
        (Json.Decode.field "username" Json.Decode.string |> Json.Decode.map Data.User.usernameFromString)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.succeed Nothing)


encode : Message -> String
encode message =
    Json.Encode.encode 0 <|
        Json.Encode.object
            [ ( "message", Json.Encode.string message.content )
            ]
