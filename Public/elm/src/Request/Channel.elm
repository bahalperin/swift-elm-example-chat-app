module Request.Channel exposing (get)

import Http
import Json.Decode
import Data.Channel exposing (Channel)


get : Http.Request (List Channel)
get =
    Http.get "/api/channels" (Json.Decode.list Data.Channel.decoder)
