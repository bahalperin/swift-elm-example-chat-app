module Main exposing (..)

import Date exposing (Date)
import Date.Format
import Dom.Scroll
import Html
    exposing
        ( Html
        , div
        , span
        , text
        , p
        , a
        , h1
        , h2
        , figure
        , img
        , form
        , input
        , button
        , header
        , section
        , footer
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , type_
        , placeholder
        , src
        , value
        , href
        )
import Html.Events
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import RemoteData exposing (RemoteData, WebData)
import Task
import Time
import WebSocket


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Initial
    | Main MainModel


type alias MainModel =
    { now : Date
    , username : String
    , messages : List Message
    , currentMessage : String
    , channels : WebData (List Channel)
    }


type alias Channels =
    { selected : Maybe Channel
    , others : List Channel
    }


type alias Channel =
    { name : String
    , messsages : WebData (List Message)
    }


type alias Message =
    { username : String
    , content : String
    , created : Maybe Date
    }



-- UPDATE


init : ( Model, Cmd Msg )
init =
    ( Initial
    , Http.get "/api/me" userDecoder
        |> Http.toTask
        |> Task.andThen
            (\username ->
                getMessages
                    |> Http.toTask
                    |> Task.map (\messages -> ( username, messages ))
            )
        |> Task.andThen
            (\( username, messages ) ->
                getChannels
                    |> Http.toTask
                    |> RemoteData.fromTask
                    |> Task.map (\channels -> ( username, messages, channels ))
            )
        |> Task.andThen
            (\( username, messages, channels ) ->
                Date.now
                    |> Task.map (\now -> ( username, messages, channels, now ))
            )
        |> Task.attempt
            (\result ->
                case result of
                    Ok payload ->
                        FacebookLogin payload

                    Err err ->
                        NoOp
            )
    )


type Msg
    = FacebookLogin ( String, List Message, WebData (List Channel), Date )
    | SetCurrentMessage String
    | ReceiveMessage String
    | SendMessage
    | GetCurrentTime
    | SetCurrentTime Date
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initial ->
            Sub.none

        Main _ ->
            Sub.batch
                [ WebSocket.listen webSocketChatUrl ReceiveMessage
                , Time.every Time.second (always GetCurrentTime)
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Main mainModel ->
            case msg of
                SetCurrentMessage message ->
                    ( Main { mainModel | currentMessage = message }, Cmd.none )

                ReceiveMessage messageJson ->
                    case decodeMessage messageJson of
                        Ok message ->
                            ( Main { mainModel | messages = List.append mainModel.messages [ { message | created = Just mainModel.now } ] }
                            , Cmd.batch
                                [ Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                                ]
                            )

                        Err err ->
                            ( model, Cmd.none )

                SendMessage ->
                    ( Main { mainModel | currentMessage = "", messages = List.append mainModel.messages [ { username = mainModel.username, content = mainModel.currentMessage, created = Just mainModel.now } ] }
                    , Cmd.batch
                        [ WebSocket.send webSocketChatUrl (encodeMessage { username = mainModel.username, content = mainModel.currentMessage, created = Just mainModel.now })
                        , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                        ]
                    )

                GetCurrentTime ->
                    ( model, Task.perform SetCurrentTime Date.now )

                SetCurrentTime time ->
                    ( Main { mainModel | now = time }, Cmd.none )

                FacebookLogin ( username, messages, channels, now ) ->
                    ( model, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

        Initial ->
            case msg of
                FacebookLogin ( username, messages, channels, now ) ->
                    ( Main
                        { username = username
                        , now = now
                        , currentMessage = ""
                        , messages = messages
                        , channels = channels
                        }
                    , Cmd.batch
                        [ joinMessage username
                            |> WebSocket.send webSocketChatUrl
                        , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Initial ->
            div [ class "app-container" ]
                [ modal True <| loginModalContent
                ]

        Main mainModel ->
            div [ class "app-container" ]
                [ div [ class "chat" ]
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
                        (List.map (viewMessage mainModel.username) mainModel.messages)
                    , div [ class "message-box" ]
                        [ form
                            [ Html.Events.onSubmit SendMessage
                            ]
                            [ input
                                [ type_ "text"
                                , class "message-input"
                                , placeholder "Type message..."
                                , value mainModel.currentMessage
                                , Html.Events.onInput SetCurrentMessage
                                ]
                                []
                            , button
                                [ type_ "submit"
                                , class "message-submit"
                                , Html.Attributes.disabled (String.length mainModel.currentMessage == 0)
                                ]
                                [ text "Send" ]
                            ]
                        ]
                    ]
                ]


modal : Bool -> Html msg -> Html msg
modal isActive content =
    div
        [ classList
            [ ( "modal", True )
            , ( "is-active", isActive )
            ]
        ]
        [ div [ class "modal-background" ] []
        , content
        ]


loginModalContent : Html Msg
loginModalContent =
    div [ class "modal-card" ]
        [ header [ class "modal-card-head" ]
            [ p [ class "modal-card-title" ]
                [ text "Login" ]
            ]
        , section [ class "modal-card-body" ]
            [ a [ href "/login/facebook " ] [ text "Facebook Login" ]
            ]
        , footer [ class "modal-card-foot" ]
            []
        ]


viewMessage : String -> Message -> Html Msg
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
            [ text <| message.username ++ ": " ++ message.content
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



-- ENCODE/DECODE


encodeMessage : Message -> String
encodeMessage message =
    Json.Encode.encode 0 <|
        Json.Encode.object
            [ ( "message", Json.Encode.string message.content )
            ]


decodeMessage : String -> Result String Message
decodeMessage messageJson =
    Json.Decode.decodeString messageDecoder messageJson


messageDecoder : Decoder Message
messageDecoder =
    Json.Decode.map3 Message
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.succeed Nothing)


joinMessage : String -> String
joinMessage username =
    Json.Encode.encode 0 <| Json.Encode.object [ ( "username", Json.Encode.string username ) ]


channelDecoder : Decoder Channel
channelDecoder =
    Json.Decode.map2 Channel
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.succeed RemoteData.NotAsked)


userDecoder : Decoder String
userDecoder =
    Json.Decode.field "username" Json.Decode.string



-- API


getMessages : Http.Request (List Message)
getMessages =
    Http.get "/api/messages" (Json.Decode.list messageDecoder)


getChannels : Http.Request (List Channel)
getChannels =
    Http.get "/api/channels" (Json.Decode.list channelDecoder)



-- MISC


webSocketChatUrl : String
webSocketChatUrl =
    "ws://localhost:8080/chat/main"
