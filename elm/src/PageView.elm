module PageView exposing (view)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background
import Element.Font
import Html exposing (Html)
import Route exposing (Route)


view : Html msg -> Document msg
view content =
    let
        header =
            Html.div []
                [ Html.a [ Route.href Route.Home ] [ Html.text "Home" ]
                , Html.text " | "
                , Html.a [ Route.href Route.CSM ] [ Html.text "CSM" ]
                ]
    in
    { title = "SRC Portal"
    , body =
        [ header
        , content
        ]
    }
