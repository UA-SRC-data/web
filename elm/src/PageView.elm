module PageView exposing (view)

--import Element exposing (..)
--import Element.Background
--import Element.Font

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (src)
import Route exposing (Route)


view : Navbar.Config msg -> Navbar.State -> Html msg -> Document msg
view navConfig navbarState content =
    let
        nav =
            navConfig
                |> Navbar.withAnimation
                |> Navbar.brand
                    [ Route.href Route.Home ]
                    [ img [ src "./assets/images/logo.png" ] [] ]
                |> Navbar.items
                    [ Navbar.itemLink
                        [ Route.href Route.Scrutinizer ]
                        [ text "Scrutinizer" ]
                    , Navbar.itemLink [ Route.href Route.CSM ] [ text "CSM" ]
                    , Navbar.itemLink
                        [ Route.href Route.GardenRoots ]
                        [ text "GardenRoots" ]
                    ]
                |> Navbar.view navbarState
    in
    { title = "SRC Portal"
    , body =
        [ Grid.container []
            [ Grid.row []
                [ Grid.col
                    []
                    [ nav
                    , content
                    ]
                ]
            ]
        ]
    }
