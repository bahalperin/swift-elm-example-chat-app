module Page.Chat exposing (..)

import Date exposing (Date)
import RemoteData exposing (WebData)
import Data.Message exposing (Message)
import Data.User exposing (Username)


type alias Model =
    { now : Date
    , username : Username
    , messages : List Message
    , currentMessage : String
    , channels : WebData (List Channel)
    }
