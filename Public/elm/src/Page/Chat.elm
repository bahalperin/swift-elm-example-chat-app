module Page.Chat exposing (Model, Msg(..), init, update, view, subscriptions)

import Dom.Scroll
import Html
    exposing
        ( Html
        , div
        , span
        , text
        , form
        , button
        , input
        , img
        , figure
        , h1
        , h2
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , type_
        , value
        , placeholder
        , src
        )
import Html.Events
import Http
import Date exposing (Date)
import Date.Format
import Json.Encode
import RemoteData exposing (WebData)
import WebSocket
import Task
import Time
import Data.Channel exposing (Channel)
import Data.Message exposing (Message)
import Data.User exposing (Username)
import Request.Message


type alias Model =
    { now : Date
    , messages : List Message
    , currentMessage : String
    , channels : WebData (List Channel)
    }


type Msg
    = SetCurrentMessage String
    | FetchMessagesResponse (Result Http.Error (List Message))
    | ReceiveMessage String
    | SendMessage
    | GetCurrentTime
    | SetCurrentTime Date
    | NoOp


init : Maybe Username -> ( Model, Cmd Msg )
init username =
    ( { now = Date.fromTime 0
      , messages = []
      , currentMessage = ""
      , channels = RemoteData.NotAsked
      }
    , Cmd.batch
        [ case username of
            Just uname ->
                joinMessage uname
                    |> WebSocket.send webSocketChatUrl

            Nothing ->
                Cmd.none
        , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
        , Request.Message.get |> Http.send FetchMessagesResponse
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen webSocketChatUrl ReceiveMessage
        , Time.every Time.second (always GetCurrentTime)
        ]


update : Maybe Username -> Msg -> Model -> ( Model, Cmd Msg )
update username msg model =
    case msg of
        SetCurrentMessage message ->
            ( { model | currentMessage = message }, Cmd.none )

        ReceiveMessage messageJson ->
            case Data.Message.decode messageJson of
                Ok message ->
                    ( { model | messages = List.append model.messages [ { message | created = Just model.now } ] }
                    , Cmd.batch
                        [ Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                        ]
                    )

                Err err ->
                    ( model, Cmd.none )

        SendMessage ->
            case username of
                Just uname ->
                    ( { model | currentMessage = "", messages = List.append model.messages [ { username = uname, content = model.currentMessage, created = Just model.now } ] }
                    , Cmd.batch
                        [ WebSocket.send webSocketChatUrl (Data.Message.encode { username = uname, content = model.currentMessage, created = Just model.now })
                        , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GetCurrentTime ->
            ( model, Task.perform SetCurrentTime Date.now )

        SetCurrentTime time ->
            ( { model | now = time }, Cmd.none )

        FetchMessagesResponse (Ok messages) ->
            ( { model | messages = messages }, Cmd.none )

        FetchMessagesResponse (Err err) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Maybe Username -> Model -> Html Msg
view username model =
    case username of
        Just uname ->
            div [ class "chat" ]
                [ div [ class "chat-title" ]
                    [ h1 []
                        [ text "Vapor Chat" ]
                    , h2 []
                        [ text "Realtime WebSocket chat powered by Vapor" ]
                    , figure [ class "avatar" ]
                        [ img
                            [ src "https://avatars3.githubusercontent.com/u/17364220?v=3&amp;s=20"
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "messages"
                    , Html.Attributes.id messageContainerId
                    ]
                    (List.map (viewMessage uname) model.messages)
                , div [ class "message-box" ]
                    [ form
                        [ Html.Events.onSubmit SendMessage
                        ]
                        [ input
                            [ type_ "text"
                            , class "message-input"
                            , placeholder "Type message..."
                            , value model.currentMessage
                            , Html.Events.onInput SetCurrentMessage
                            ]
                            []
                        , button
                            [ type_ "submit"
                            , class "message-submit"
                            , Html.Attributes.disabled (String.length model.currentMessage == 0)
                            ]
                            [ text "Send" ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


viewMessage : Username -> Message -> Html Msg
viewMessage username message =
    div
        [ classList
            [ ( "message", True )
            , ( "new", True )
            , ( "personal", message.username == username )
            ]
        ]
        [ img
            [ class "avatar"
            , src "https://avatars3.githubusercontent.com/u/17364220?v=3&amp;s=20"
            ]
            []
        , span [ class "text" ]
            [ text <| (Data.User.usernameToString message.username) ++ ": " ++ message.content
            ]
        , case message.created of
            Just date ->
                span [ class "timestamp" ]
                    [ text <| Date.Format.format "%H:%M" date
                    ]

            Nothing ->
                text ""
        ]


messageContainerId : String
messageContainerId =
    "messages"


joinMessage : Username -> String
joinMessage username =
    Json.Encode.encode 0 <| Json.Encode.object [ ( "username", Json.Encode.string <| Data.User.usernameToString username ) ]


webSocketChatUrl : String
webSocketChatUrl =
    "ws://localhost:8080/chat/main"
