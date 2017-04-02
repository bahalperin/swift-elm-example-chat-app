module Main exposing (..)

import Date exposing (Date)
import Date.Format
import Dict exposing (Dict)
import Dom.Scroll
import Html
    exposing
        ( Html
        , div
        , span
        , text
        , p
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
        )
import Html.Events
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
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


type alias Model =
    { now : Maybe Date
    , usernameField : String
    , username : Maybe String
    , avatarLookup : Dict String String
    , messages : List Message
    , currentMessage : String
    }


type alias Message =
    { username : String
    , content : String
    , created : Maybe Date
    }



-- UPDATE


init : ( Model, Cmd Msg )
init =
    ( { now = Nothing
      , usernameField = ""
      , username = Nothing
      , avatarLookup = Dict.empty
      , messages = []
      , currentMessage = ""
      }
    , Cmd.none
    )


type Msg
    = SetUsernameField String
    | SetUsernameResponse (Result Http.Error String)
    | AttemptToSetUsername
    | AddAvatarResponse String (Result Http.Error String)
    | SetCurrentMessage String
    | ReceiveMessage String
    | SendMessage
    | GetCurrentTime
    | SetCurrentTime Date
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen webSocketChatUrl ReceiveMessage
        , Time.every Time.second (always GetCurrentTime)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsernameField username ->
            ( { model | usernameField = username }, Cmd.none )

        SetUsernameResponse (Ok avatarUrl) ->
            ( { model
                | username = Just model.usernameField
                , usernameField = ""
                , avatarLookup = Dict.insert model.usernameField avatarUrl model.avatarLookup
              }
            , WebSocket.send webSocketChatUrl <| joinMessage model.usernameField
            )

        SetUsernameResponse (Err _) ->
            ( model, Cmd.none )

        AttemptToSetUsername ->
            ( model
            , getGithubUserRequest model.usernameField
                |> Http.send SetUsernameResponse
            )

        AddAvatarResponse username (Ok avatarUrl) ->
            ( { model | avatarLookup = Dict.insert username avatarUrl model.avatarLookup }
            , Cmd.none
            )

        AddAvatarResponse username (Err _) ->
            ( model, Cmd.none )

        SetCurrentMessage message ->
            ( { model | currentMessage = message }, Cmd.none )

        ReceiveMessage messageJson ->
            case decodeMessage messageJson of
                Ok message ->
                    ( { model | messages = List.append model.messages [ { message | created = model.now } ] }
                    , Cmd.batch
                        [ Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom "messages")
                        , case Dict.get message.username model.avatarLookup of
                            Just _ ->
                                Cmd.none

                            Nothing ->
                                getGithubUserRequest message.username
                                    |> Http.send (AddAvatarResponse message.username)
                        ]
                    )

                Err err ->
                    ( model, Cmd.none )

        SendMessage ->
            ( { model | currentMessage = "", messages = List.append model.messages [ { username = Maybe.withDefault "" model.username, content = model.currentMessage, created = model.now } ] }
            , Cmd.batch
                [ WebSocket.send webSocketChatUrl (encodeMessage { username = Maybe.withDefault "" model.username, content = model.currentMessage, created = model.now })
                , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom "messages")
                ]
            )

        GetCurrentTime ->
            ( model, Task.perform SetCurrentTime Date.now )

        SetCurrentTime time ->
            ( { model | now = Just time }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
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
                , Html.Attributes.id "messages"
                ]
                (List.map (viewMessage (Maybe.withDefault "" model.username) model.avatarLookup) model.messages)
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
        , modal (model.username == Nothing) <| loginModalContent model
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


loginModalContent : Model -> Html Msg
loginModalContent model =
    div [ class "modal-card" ]
        [ header [ class "modal-card-head" ]
            [ p [ class "modal-card-title" ]
                [ text "Login" ]
            ]
        , section [ class "modal-card-body" ]
            [ form
                [ Html.Events.onSubmit AttemptToSetUsername
                ]
                [ input
                    [ type_ "text"
                    , value model.usernameField
                    , placeholder "Enter your Github username"
                    , class "input"
                    , Html.Attributes.autofocus True
                    , Html.Events.onInput SetUsernameField
                    ]
                    []
                , button
                    [ type_ "submit"
                    , class "button"
                    ]
                    [ text "Join Chat" ]
                ]
            ]
        , footer [ class "modal-card-foot" ]
            []
        ]


viewMessage : String -> Dict String String -> Message -> Html Msg
viewMessage username avatarLookup message =
    div
        [ classList
            [ ( "message", True )
            , ( "new", True )
            , ( "personal", message.username == username )
            ]
        ]
        [ img
            [ class "avatar"
            , case Dict.get message.username avatarLookup of
                Just avatarUrl ->
                    src avatarUrl

                Nothing ->
                    src "https://avatars3.githubusercontent.com/u/17364220?v=3&amp;s=20"
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
        (Json.Decode.field "message" Json.Decode.string)
        (Json.Decode.succeed Nothing)


joinMessage : String -> String
joinMessage username =
    Json.Encode.encode 0 <| Json.Encode.object [ ( "username", Json.Encode.string username ) ]



-- COMMANDS


getGithubUserRequest : String -> Http.Request String
getGithubUserRequest username =
    Http.get ("https://api.github.com/users/" ++ username) (Json.Decode.field "avatar_url" Json.Decode.string)



-- MISC


webSocketChatUrl : String
webSocketChatUrl =
    "ws://localhost:8080/chat"
