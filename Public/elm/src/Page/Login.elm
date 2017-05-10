module Page.Login exposing (Msg(..), ExternalMsg(..), modalContent)

import Html
    exposing
        ( Html
        , div
        , header
        , a
        , p
        , text
        , section
        , footer
        )
import Html.Attributes
    exposing
        ( class
        , href
        )


type Msg
    = NoOp


type ExternalMsg
    = Login


modalContent : Html msg
modalContent =
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
