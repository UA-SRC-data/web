module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CSM
import Page.GardenRoots
import Page.Home
import Page.Scrutinizer
import PageView
import Route exposing (Route)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Page
    = Home
    | CSM Page.CSM.Model
    | GardenRoots
    | Scrutinizer Page.Scrutinizer.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , cur_page : Page
    , navbarState : Navbar.State
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( Model key url Home navbarState, navbarCmd )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | CSMMsg Page.CSM.Msg
    | GardenRootsMsg
    | NavbarMsg Navbar.State
    | UrlChanged Url.Url
    | ScrutinizerMsg Page.Scrutinizer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.cur_page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( NavbarMsg state, _ ) ->
            ( { model | navbarState = state }, Cmd.none )

        ( ScrutinizerMsg subMsg, Scrutinizer subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Scrutinizer.update subMsg subModel
            in
            ( { model | cur_page = Scrutinizer newSubModel }, Cmd.map ScrutinizerMsg newCmd )

        ( CSMMsg subMsg, CSM subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.CSM.update subMsg subModel
            in
            ( { model | cur_page = CSM newSubModel }, Cmd.map CSMMsg newCmd )

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Just Route.Scrutinizer ->
            let
                ( subModel, subMsg ) =
                    Page.Scrutinizer.init
            in
            ( { model | cur_page = Scrutinizer subModel }, Cmd.map ScrutinizerMsg subMsg )

        Just Route.CSM ->
            let
                ( subModel, subMsg ) =
                    Page.CSM.init
            in
            ( { model | cur_page = CSM subModel }, Cmd.map CSMMsg subMsg )

        Just Route.GardenRoots ->
            ( { model | cur_page = GardenRoots }, Cmd.none )

        _ ->
            ( { model | cur_page = Home }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.cur_page of
        Scrutinizer subModel ->
            Sub.map ScrutinizerMsg (Page.Scrutinizer.subscriptions subModel)

        CSM subModel ->
            Sub.map CSMMsg (Page.CSM.subscriptions subModel)

        _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        navConfig =
            Navbar.config NavbarMsg
    in
    case model.cur_page of
        Home ->
            PageView.view navConfig model.navbarState Page.Home.view

        GardenRoots ->
            PageView.view navConfig model.navbarState Page.GardenRoots.view

        CSM subModel ->
            PageView.view navConfig
                model.navbarState
                (Html.map CSMMsg (Page.CSM.view subModel))

        Scrutinizer subModel ->
            PageView.view navConfig
                model.navbarState
                (Html.map ScrutinizerMsg (Page.Scrutinizer.view subModel))



--EJScreen subModel ->
--    PageView.view navConfig model.navbarState (Html.map EJScreenMsg (Page.EJScreen.view subModel))
