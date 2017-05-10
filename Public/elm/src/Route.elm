module Route
    exposing
        ( Route(..)
        , href
        , modifyUrl
        , fromLocation
        )

import Navigation exposing (Location)
import UrlParser exposing (Parser, s, (</>))
import Html exposing (Attribute)
import Html.Attributes as Attr


type Route
    = Login
    | Chat String
    | Unknown String


route : Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Login (s "login")
        , UrlParser.map Chat (s "channel" </> UrlParser.string)
        , UrlParser.map Unknown UrlParser.string
        ]


routeToString : Route -> String
routeToString page =
    page
        |> routeToStringHelp
        |> String.join "/"
        |> (\str -> "/#" ++ str)


routeToStringHelp : Route -> List String
routeToStringHelp page =
    case page of
        Chat channel ->
            [ "channel", channel ]

        Login ->
            [ "login" ]

        Unknown url ->
            [ url ]


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Route
fromLocation location =
    UrlParser.parseHash route location
        |> Maybe.withDefault (Unknown location.hash)
