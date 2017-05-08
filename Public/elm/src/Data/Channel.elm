module Data.Channel exposing (Channel, decoder)

import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import Data.Message exposing (Message)


type alias Channel =
    { name : String
    , messsages : WebData (List Message)
    }


decoder : Decoder Channel
decoder =
    Json.Decode.map2 Channel
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.succeed RemoteData.NotAsked)
