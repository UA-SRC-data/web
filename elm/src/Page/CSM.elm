module Page.CSM exposing (Model, Msg, init, update, view)

import Config exposing (apiServer)
import Html exposing (Html, div, h1, h2, p, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData, WebData)


type alias Model =
    { records : WebData (List Record) }


type alias Record =
    { collection_date : String
    , measurement : String
    , station : String
    , val : Float
    }


type Msg
    = MakeRequest
    | DataResponse (WebData (List Record))


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.Loading }, getData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeRequest ->
            ( { model | records = RemoteData.Loading }, getData )

        DataResponse data ->
            ( { model | records = data }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "SRC Data" ]
        , viewData model
        ]


viewData : Model -> Html Msg
viewData model =
    case model.records of
        RemoteData.NotAsked ->
            div [] [ text "Not asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading data..." ]

        RemoteData.Failure httpError ->
            div [] [ text (buildErrorMessage httpError) ]

        RemoteData.Success data ->
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
        , expect = Http.expectJson (RemoteData.fromResult >> DataResponse) (Json.Decode.list dataDecoder)
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


dataDecoder : Decoder Record
dataDecoder =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "collection_date" string
        |> Json.Decode.Pipeline.required "measurement" string
        |> Json.Decode.Pipeline.required "station" string
        |> Json.Decode.Pipeline.required "val" float
