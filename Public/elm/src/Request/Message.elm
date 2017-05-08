module Request.Message exposing (get)

import Http
import Json.Decode exposing (Decoder)
import Data.Message exposing (Message)


get : Http.Request (List Message)
get =
    Http.get "/api/messages" (Json.Decode.list Data.Message.decoder)
