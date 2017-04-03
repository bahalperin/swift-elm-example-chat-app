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


type Model
    = Initial InitialModel
    | Main MainModel


type alias InitialModel =
    { usernameField : String
    }


type alias MainModel =
    { now : Date
    , username : String
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
    ( Initial
        { usernameField = ""
        }
    , Cmd.none
    )


type Msg
    = Login LoginMsg
    | App AppMsg
    | Transition TransitionMsg


type LoginMsg
    = SetUsernameField String
    | LoginFail Http.Error


type TransitionMsg
    = LoginSuccess ( Date, String, List Message )
    | AttemptToLogin


type AppMsg
    = AddAvatarResponse String (Result Http.Error String)
    | SetCurrentMessage String
    | ReceiveMessage String
    | SendMessage
    | GetCurrentTime
    | SetCurrentTime Date
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initial _ ->
            Sub.none

        Main _ ->
            Sub.map App <|
                Sub.batch
                    [ WebSocket.listen webSocketChatUrl ReceiveMessage
                    , Time.every Time.second (always GetCurrentTime)
                    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Transition _ ->
            transitionModel msg model

        Login loginMsg ->
            case model of
                Initial initialModel ->
                    updateInitialModel loginMsg initialModel
                        |> (\( newModel, newCmd ) -> ( Initial newModel, Cmd.map Login newCmd ))

                _ ->
                    ( model, Cmd.none )

        App appMsg ->
            case model of
                Main mainModel ->
                    updateMainModel appMsg mainModel
                        |> (\( newModel, newCmd ) -> ( Main newModel, Cmd.map App newCmd ))

                _ ->
                    ( model, Cmd.none )


updateInitialModel : LoginMsg -> InitialModel -> ( InitialModel, Cmd LoginMsg )
updateInitialModel msg model =
    case msg of
        SetUsernameField username ->
            ( { model | usernameField = username }, Cmd.none )

        LoginFail err ->
            ( model, Cmd.none )


updateMainModel : AppMsg -> MainModel -> ( MainModel, Cmd AppMsg )
updateMainModel msg model =
    case msg of
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
                    ( { model | messages = List.append model.messages [ { message | created = Just model.now } ] }
                    , Cmd.batch
                        [ Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
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
            ( { model | currentMessage = "", messages = List.append model.messages [ { username = model.username, content = model.currentMessage, created = Just model.now } ] }
            , Cmd.batch
                [ WebSocket.send webSocketChatUrl (encodeMessage { username = model.username, content = model.currentMessage, created = Just model.now })
                , Task.attempt (\_ -> NoOp) (Dom.Scroll.toBottom messageContainerId)
                ]
            )

        GetCurrentTime ->
            ( model, Task.perform SetCurrentTime Date.now )

        SetCurrentTime time ->
            ( { model | now = time }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


transitionModel : Msg -> Model -> ( Model, Cmd Msg )
transitionModel msg model =
    case model of
        Initial initialModel ->
            case msg of
                Transition transitionMsg ->
                    case transitionMsg of
                        LoginSuccess ( now, avatarUrl, messages ) ->
                            ( Main
                                { username = initialModel.usernameField
                                , avatarLookup = Dict.fromList [ ( initialModel.usernameField, avatarUrl ) ]
                                , now = now
                                , currentMessage = ""
                                , messages = messages
                                }
                            , joinMessage initialModel.usernameField
                                |> WebSocket.send webSocketChatUrl
                                |> Cmd.map App
                            )

                        AttemptToLogin ->
                            ( Initial initialModel
                            , getGithubUserRequest initialModel.usernameField
                                |> Http.toTask
                                |> Task.andThen
                                    (\avatarUrl ->
                                        Date.now
                                            |> Task.map (\now -> ( now, avatarUrl ))
                                    )
                                |> Task.andThen
                                    (\( now, avatarUrl ) ->
                                        getMessages
                                            |> Http.toTask
                                            |> Task.map (\messages -> ( now, avatarUrl, messages ))
                                    )
                                |> Task.attempt
                                    (\result ->
                                        case result of
                                            Ok payload ->
                                                Transition <| LoginSuccess payload

                                            Err err ->
                                                Login <| LoginFail err
                                    )
                            )

                _ ->
                    ( model, Cmd.none )

        Main _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Initial initialModel ->
            div [ class "app-container" ]
                [ modal True <| loginModalContent initialModel
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
                        (List.map (viewMessage mainModel.username mainModel.avatarLookup) mainModel.messages)
                    , div [ class "message-box" ]
                        [ form
                            [ Html.Events.onSubmit (App SendMessage)
                            ]
                            [ input
                                [ type_ "text"
                                , class "message-input"
                                , placeholder "Type message..."
                                , value mainModel.currentMessage
                                , Html.Events.onInput (App << SetCurrentMessage)
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


loginModalContent : InitialModel -> Html Msg
loginModalContent model =
    div [ class "modal-card" ]
        [ header [ class "modal-card-head" ]
            [ p [ class "modal-card-title" ]
                [ text "Login" ]
            ]
        , section [ class "modal-card-body" ]
            [ form
                [ Html.Events.onSubmit (Transition AttemptToLogin)
                ]
                [ input
                    [ type_ "text"
                    , value model.usernameField
                    , placeholder "Enter your Github username"
                    , class "input"
                    , Html.Attributes.autofocus True
                    , Html.Events.onInput (Login << SetUsernameField)
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



-- API


getGithubUserRequest : String -> Http.Request String
getGithubUserRequest username =
    Http.get ("https://api.github.com/users/" ++ username) (Json.Decode.field "avatar_url" Json.Decode.string)


getMessages : Http.Request (List Message)
getMessages =
    Http.get "/api/messages" (Json.Decode.list messageDecoder)



-- MISC


webSocketChatUrl : String
webSocketChatUrl =
    "ws://localhost:8080/chat"
