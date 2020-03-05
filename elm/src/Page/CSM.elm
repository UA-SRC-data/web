module Page.CSM exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Config exposing (apiServer)
import Html exposing (Html, div, h1, h2, input, p, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import HttpBuilder
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData, WebData)
import Table
import Url.Builder


type alias Model =
    { records : WebData (List Record)
    , measurements : WebData (List Measurement)
    , stations : WebData (List Station)
    , tableState : Table.State
    , query : String
    , selectedMeasurement : Maybe String
    , selectedStation : Maybe String
    , minValue : Maybe Int
    , maxValue : Maybe Int
    }


type alias Measurement =
    { measurement : String
    }


type alias Station =
    { station : String
    }


type alias Record =
    { collection_date : String
    , measurement : String
    , station : String
    , val : Float
    }


type Msg
    = MakeRequest
    | DataResponse (WebData (List Record))
    | MeasurementsResponse (WebData (List Measurement))
    | StationsResponse (WebData (List Station))
    | SetQuery String
    | SetTableState Table.State
    | SelectMeasurement String
    | SelectStation String
    | SetMinValue String
    | SetMaxValue String


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.NotAsked
      , measurements = RemoteData.NotAsked
      , stations = RemoteData.NotAsked
      , tableState = Table.initialSort "Year"
      , query = ""
      , selectedMeasurement = Nothing
      , selectedStation = Nothing
      , minValue = Nothing
      , maxValue = Nothing
      }
    , Cmd.batch
        [ getMeasurements, getStations ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeRequest ->
            ( { model | records = RemoteData.Loading }, getData model )

        DataResponse data ->
            ( { model | records = data }
            , Cmd.none
            )

        MeasurementsResponse data ->
            ( { model | measurements = data }
            , Cmd.none
            )

        StationsResponse data ->
            ( { model | stations = data }
            , Cmd.none
            )

        SelectMeasurement measurement ->
            let
                newMeasurement =
                    case measurement of
                        "" ->
                            Nothing

                        _ ->
                            Just measurement
            in
            ( { model | selectedMeasurement = newMeasurement }, Cmd.none )

        SelectStation station ->
            let
                newStation =
                    case station of
                        "" ->
                            Nothing

                        _ ->
                            Just station
            in
            ( { model | selectedStation = newStation }, Cmd.none )

        SetMaxValue max ->
            ( { model | maxValue = String.toInt max }, Cmd.none )

        SetMinValue min ->
            ( { model | minValue = String.toInt min }, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Colorado School of Mines Data" ]
        , viewForm model
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
            div [] [ text (viewHttpErrorMessage httpError) ]

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

                lowerQuery =
                    String.toLower model.query

                filtered =
                    List.filter
                        (String.contains lowerQuery
                            << String.toLower
                            << .measurement
                        )
                        data
            in
            div []
                [ text ("Showing " ++ String.fromInt (List.length data))
                , Table.view tblConfig model.tableState filtered
                ]


viewForm : Model -> Html Msg
viewForm model =
    Form.form []
        [ measurementsSelect model.measurements
        , stationsSelect model.stations
        , countMin
        , countMax
        , Button.button
            [ Button.onClick MakeRequest
            , Button.primary
            ]
            [ text "Submit" ]
        ]


countMin : Html Msg
countMin =
    Form.group []
        [ Form.label [ for "min_value" ] [ text "Min Value" ]
        , Input.number [ Input.onInput SetMinValue ]
        ]


countMax : Html Msg
countMax =
    Form.group []
        [ Form.label [ for "max_value" ] [ text "Max Value" ]
        , Input.number [ Input.onInput SetMaxValue ]
        ]


measurementsSelect : WebData (List Measurement) -> Html Msg
measurementsSelect measurements =
    case measurements of
        RemoteData.NotAsked ->
            div [] [ text "Not asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Failure httpError ->
            div [] [ text (viewHttpErrorMessage httpError) ]

        RemoteData.Success data ->
            let
                empty =
                    Measurement ""

                makeItem item =
                    Select.item
                        [ value item.measurement ]
                        [ text item.measurement ]
            in
            Form.group []
                [ Form.label [ for "measurement" ] [ text "Measurement" ]
                , Select.select
                    [ Select.id "measurements"
                    , Select.onChange SelectMeasurement
                    ]
                    (List.map makeItem ([ empty ] ++ data))
                ]


stationsSelect : WebData (List Station) -> Html Msg
stationsSelect stations =
    case stations of
        RemoteData.NotAsked ->
            div [] [ text "Not asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Failure httpError ->
            div [] [ text (viewHttpErrorMessage httpError) ]

        RemoteData.Success data ->
            let
                empty =
                    Station ""

                makeItem item =
                    Select.item
                        [ value item.station ]
                        [ text item.station ]
            in
            Form.group []
                [ Form.label [ for "station" ] [ text "Station" ]
                , Select.select
                    [ Select.id "stations", Select.onChange SelectStation ]
                    (List.map makeItem ([ empty ] ++ data))
                ]


viewHttpErrorMessage : Http.Error -> String
viewHttpErrorMessage httpError =
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


getData : Model -> Cmd Msg
getData model =
    let
        maybeToString m =
            case m of
                Nothing ->
                    ""

                Just s ->
                    s

        maybeToInt m =
            case m of
                Nothing ->
                    ""

                Just n ->
                    String.fromInt n

        queryParams =
            Url.Builder.toQuery
                [ Url.Builder.string
                    "measurement"
                    (maybeToString model.selectedMeasurement)
                , Url.Builder.string
                    "station"
                    (maybeToString model.selectedStation)
                , Url.Builder.string
                    "val_min"
                    (maybeToInt model.minValue)
                , Url.Builder.string
                    "val_max"
                    (maybeToInt model.maxValue)
                ]

        url =
            apiServer ++ "/data/csm" ++ queryParams
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect
            (Http.expectJson
                (RemoteData.fromResult >> DataResponse)
                (Json.Decode.list decoderData)
            )
        |> HttpBuilder.request


getMeasurements : Cmd Msg
getMeasurements =
    let
        url =
            apiServer ++ "/data/csm/measurements"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> MeasurementsResponse)
                (Json.Decode.list decoderMeasurement)
        }


getStations : Cmd Msg
getStations =
    let
        url =
            apiServer ++ "/data/csm/stations"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> StationsResponse)
                (Json.Decode.list decoderStation)
        }


tblConfig : Table.Config Record Msg
tblConfig =
    Table.config
        { toId = .station
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Collected" .collection_date
            , Table.stringColumn "Measurement" .measurement
            , Table.stringColumn "Station" .station
            , Table.floatColumn "Value" .val
            ]
        }


decoderData : Decoder Record
decoderData =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "collection_date" string
        |> Json.Decode.Pipeline.required "measurement" string
        |> Json.Decode.Pipeline.required "station" string
        |> Json.Decode.Pipeline.required "val" float


decoderMeasurement : Decoder Measurement
decoderMeasurement =
    Json.Decode.succeed Measurement
        |> Json.Decode.Pipeline.required "measurement" string


decoderStation : Decoder Station
decoderStation =
    Json.Decode.succeed Station
        |> Json.Decode.Pipeline.required "station" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
