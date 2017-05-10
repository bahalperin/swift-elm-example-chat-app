module Main exposing (..)

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
import Http
import Navigation
import Task
import Data.User exposing (Username)
import Route exposing (Route(..))
import Page.Chat
import Page.Login


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions =
            (\model ->
                case model.page of
                    Chat chatModel ->
                        Sub.map ChatMsg (Page.Chat.subscriptions chatModel)

                    _ ->
                        Sub.none
            )
        }



-- MODEL


type alias Model =
    { session : { username : Maybe Username }
    , page : Page
    }


type Page
    = Login
    | Chat Page.Chat.Model
    | Unknown



-- UPDATE


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { session = { username = Nothing }
      , page = Login
      }
    , Http.get "/api/me" Data.User.decoder
        |> Http.toTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok username ->
                        SetUser username

                    Err err ->
                        NoOp
            )
    )



{--
        |> Task.andThen
            (\username ->
                Request.Message.get
                    |> Http.toTask
                    |> Task.map (\messages -> ( username, messages ))
            )
        |> Task.andThen
            (\( username, messages ) ->
                Request.Channel.get
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
--}


type Msg
    = ChatMsg Page.Chat.Msg
    | SetRoute Route
    | SetUser String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( Chat chatModel, ChatMsg chatMsg ) ->
            let
                ( newChatModel, chatCmd ) =
                    Page.Chat.update model.session.username chatMsg chatModel
            in
                ( { model | page = Chat newChatModel }, Cmd.map ChatMsg chatCmd )

        ( _, SetUser username ) ->
            let
                session =
                    model.session
            in
                ( { model
                    | session = { session | username = Just <| Data.User.usernameFromString username }
                  }
                , Route.modifyUrl (Route.Chat "main")
                )

        ( _, SetRoute route ) ->
            case route of
                Route.Login ->
                    ( { model | page = Login }, Cmd.none )

                Route.Chat _ ->
                    let
                        ( chatPage, chatCmd ) =
                            Page.Chat.init model.session.username
                    in
                        ( { model | page = Chat chatPage }
                        , Cmd.map ChatMsg chatCmd
                        )

                Route.Unknown url ->
                    ( { model | page = Unknown }, Cmd.none )

        ( _, NoOp ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Login ->
            div [ class "app-container" ]
                [ modal True <| Page.Login.modalContent
                ]

        Chat mainModel ->
            div [ class "app-container" ]
                [ Page.Chat.view model.session.username mainModel |> Html.map ChatMsg
                ]

        Unknown ->
            text "Bad url"


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
