module Main exposing (Model(..), Msg(..), dataDecoder, getData, init, main, subscriptions, update, view, viewData)

--import HttpBuilder

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)



-- MAIN


apiServer =
    "http://127.0.0.1:8000"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Record =
    { collection_date : String
    , measurement : String
    , station : String
    , val : Float
    }



-- MODEL


type Model
    = Failure String
    | Loading
    | Success (List Record)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getData )



-- UPDATE


type Msg
    = MorePlease
    | GotData (Result Http.Error (List Record))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getData )

        GotData result ->
            case result of
                Ok data ->
                    ( Success data, Cmd.none )

                Err err ->
                    let
                        text =
                            case err of
                                Http.BadUrl m ->
                                    "Bad URL: " ++ m

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "Network Error"

                                Http.BadStatus _ ->
                                    "Bad status"

                                Http.BadBody body ->
                                    "Bad body: " ++ body
                    in
                    ( Failure text, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "SRC Data" ]
        , viewData model
        ]


viewData : Model -> Html Msg
viewData model =
    case model of
        Failure err ->
            div [] [ text ("Unable to data: " ++ err) ]

        Loading ->
            text "Loading..."

        Success data ->
            let
                viewRec rec =
                    tr []
                        [ td [] [ text rec.collection_date ]
                        , td [] [ text rec.measurement ]
                        , td [] [ text rec.station ]
                        , td [ style "text-align" "right" ]
                            [ text (String.fromFloat rec.val) ]
                        ]

                header =
                    tr []
                        [ th [] [ text "Collected" ]
                        , th [] [ text "Measurement" ]
                        , th [] [ text "Station" ]
                        , th [] [ text "Value" ]
                        ]
            in
            table [] ([ header ] ++ List.map viewRec data)



-- HTTP


getData : Cmd Msg
getData =
    let
        decoder =
            Json.Decode.list dataDecoder

        url =
            apiServer ++ "/data/csm"

        _ =
            Debug.log ("url = " ++ url)
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotData decoder
        }


dataDecoder : Decoder Record
dataDecoder =
    Json.Decode.succeed Record
        |> required "collection_date" string
        |> required "measurement" string
        |> required "station" string
        |> required "val" float
