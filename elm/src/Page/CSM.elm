module Page.CSM exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Config exposing (apiServer)
import Html exposing (Html, div, h1, h2, input, p, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData, WebData)
import Table


type alias Model =
    { records : WebData (List Record)
    , measurements : WebData (List Measurement)
    , tableState : Table.State
    , query : String
    , selectedMeasurement : Maybe String
    }


type alias Measurement =
    { measurement : String
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
    | MeasurementResponse (WebData (List Measurement))
    | SetQuery String
    | SetTableState Table.State
    | SelectMeasurement String


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.NotAsked
      , measurements = RemoteData.Loading
      , tableState = Table.initialSort "Year"
      , query = ""
      , selectedMeasurement = Nothing
      }
    , getMeasurements
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeRequest ->
            ( { model | records = RemoteData.Loading }, Cmd.none )

        DataResponse data ->
            ( { model | records = data }
            , Cmd.none
            )

        MeasurementResponse data ->
            ( { model | measurements = data }
            , Cmd.none
            )

        SelectMeasurement measurement ->
            let
                ( newMeasurement, data ) =
                    case measurement of
                        "" ->
                            ( Nothing, RemoteData.Loading )

                        _ ->
                            ( Just measurement, RemoteData.Loading )

                newModel =
                    { model
                        | selectedMeasurement = newMeasurement
                        , records = data
                    }
            in
            ( newModel, getData newModel )

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
        , viewMeasurements model
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
                , input [ placeholder "Search by Name", onInput SetQuery ] []
                , Table.view tblConfig model.tableState filtered
                ]


viewMeasurements : Model -> Html Msg
viewMeasurements model =
    let
        f m =
            text m.measurement

        body =
            case model.measurements of
                RemoteData.NotAsked ->
                    div [] [ text "Not asked" ]

                RemoteData.Loading ->
                    div [] [ text "Loading" ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success data ->
                    measurementsSelect data
    in
    Form.form [] [ body ]


measurementsSelect : List Measurement -> Html Msg
measurementsSelect measurements =
    let
        empty =
            Measurement ""

        makeItem item =
            Select.item [ value item.measurement ] [ text item.measurement ]
    in
    Form.group []
        [ Form.label [ for "measurement" ] [ text "Measurement" ]
        , Select.select
            [ Select.id "myselect", Select.onChange SelectMeasurement ]
            (List.map makeItem ([ empty ] ++ measurements))
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
        decoder =
            Json.Decode.list dataDecoder

        selectedParam =
            case model.selectedMeasurement of
                Nothing ->
                    ""

                Just m ->
                    "measurement=" ++ m

        url =
            apiServer ++ "/data/csm?" ++ selectedParam
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> DataResponse)
                (Json.Decode.list dataDecoder)
        }


getMeasurements : Cmd Msg
getMeasurements =
    let
        decoder =
            Json.Decode.list measurementDecoder

        url =
            apiServer ++ "/data/csm/measurements"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> MeasurementResponse)
                (Json.Decode.list measurementDecoder)
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


dataDecoder : Decoder Record
dataDecoder =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "collection_date" string
        |> Json.Decode.Pipeline.required "measurement" string
        |> Json.Decode.Pipeline.required "station" string
        |> Json.Decode.Pipeline.required "val" float


measurementDecoder : Decoder Measurement
measurementDecoder =
    Json.Decode.succeed Measurement
        |> Json.Decode.Pipeline.required "measurement" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
