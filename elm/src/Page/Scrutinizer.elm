module Page.Scrutinizer exposing (Model, Msg, init, subscriptions, update, view)

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
    , query : String
    , tableState : Table.State
    , variables : WebData (List Variable)
    , minValue : Maybe Int
    , maxValue : Maybe Int
    , selectedVariable : Maybe String
    }


type alias Variable =
    { variable : String
    , description : String
    }


type alias Record =
    { variable : String
    , location_name : String
    , location_type : String
    , collected_on : String
    , value : Float
    }


type Msg
    = MakeRequest
    | DataResponse (WebData (List Record))
      -- | MeasurementsResponse (WebData (List Measurement))
    | VariablesResponse (WebData (List Variable))
    | SelectVariable String
    | SetQuery String
    | SetTableState Table.State
    | SetMinValue String
    | SetMaxValue String


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.NotAsked
      , tableState = Table.initialSort "Year"
      , query = ""
      , minValue = Nothing
      , maxValue = Nothing
      , variables = RemoteData.NotAsked
      , selectedVariable = Nothing
      }
    , Cmd.batch [ getVariables ]
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

        --MeasurementsResponse data ->
        --    ( { model | measurements = data }
        --    , Cmd.none
        --    )
        VariablesResponse data ->
            ( { model | variables = data }
            , Cmd.none
            )

        SelectVariable variable ->
            let
                newVariable =
                    case variable of
                        "" ->
                            Nothing

                        _ ->
                            Just variable
            in
            ( { model | selectedVariable = newVariable }, Cmd.none )

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
        [ h2 [] [ text "Central Scrutinizer" ]
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
                lowerQuery =
                    String.toLower model.query

                filtered =
                    List.filter
                        (String.contains lowerQuery
                            << String.toLower
                            << .variable
                        )
                        data
            in
            div [ style "width" "100%" ]
                [ text ("Showing " ++ String.fromInt (List.length data))
                , Table.view tblConfig model.tableState filtered
                ]


viewForm : Model -> Html Msg
viewForm model =
    Form.form []
        [ variableSelect model.variables
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


variableSelect : WebData (List Variable) -> Html Msg
variableSelect variables =
    case variables of
        RemoteData.NotAsked ->
            div [] [ text "Not asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Failure httpError ->
            div [] [ text (viewHttpErrorMessage httpError) ]

        RemoteData.Success data ->
            let
                empty =
                    Variable "" "--Choose--"

                display item =
                    case String.length item.variable of
                        0 ->
                            item.description

                        _ ->
                            item.description
                                ++ " ("
                                ++ item.variable
                                ++ ")"

                makeItem item =
                    Select.item
                        [ value item.variable ]
                        [ text (display item) ]

                sortedData =
                    List.sortBy .description data
            in
            Form.group []
                [ Form.label [ for "variable" ] [ text "Variable" ]
                , Select.select
                    [ Select.id "variables"
                    , Select.onChange SelectVariable
                    ]
                    (List.map makeItem ([ empty ] ++ sortedData))
                ]



--stationsSelect : WebData (List Station) -> Html Msg
--stationsSelect stations =
--    case stations of
--        RemoteData.NotAsked ->
--            div [] [ text "Not asked" ]
--        RemoteData.Loading ->
--            div [] [ text "Loading" ]
--        RemoteData.Failure httpError ->
--            div [] [ text (viewHttpErrorMessage httpError) ]
--        RemoteData.Success data ->
--            let
--                empty =
--                    Station ""
--                makeItem item =
--                    Select.item
--                        [ value item.station ]
--                        [ text item.station ]
--            in
--            Form.group []
--                [ Form.label [ for "station" ] [ text "Station" ]
--                , Select.select
--                    [ Select.id "stations", Select.onChange SelectStation ]
--                    (List.map makeItem ([ empty ] ++ data))
--                ]


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
                    "variable"
                    (maybeToString model.selectedVariable)
                , Url.Builder.string
                    "min_value"
                    (maybeToInt model.minValue)
                , Url.Builder.string
                    "max_value"
                    (maybeToInt model.maxValue)
                ]

        url =
            apiServer ++ "/scrutinizer/measurements" ++ queryParams
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect
            (Http.expectJson
                (RemoteData.fromResult >> DataResponse)
                (Json.Decode.list decoderData)
            )
        |> HttpBuilder.request


getVariables : Cmd Msg
getVariables =
    let
        url =
            apiServer ++ "/scrutinizer/variables"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> VariablesResponse)
                (Json.Decode.list decoderVariable)
        }


tblConfig : Table.Config Record Msg
tblConfig =
    Table.config
        { toId = .variable
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Collected" .collected_on
            , Table.stringColumn "Variable" .variable
            , Table.stringColumn "Location" .location_name
            , Table.stringColumn "Location Type" .location_type
            , Table.floatColumn "Value" .value
            ]
        }


decoderData : Decoder Record
decoderData =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "collected_on" string
        |> Json.Decode.Pipeline.required "variable" string
        |> Json.Decode.Pipeline.required "location_name" string
        |> Json.Decode.Pipeline.required "location_type" string
        |> Json.Decode.Pipeline.required "value" float



--decoderMeasurement : Decoder Measurement
--decoderMeasurement =
--    Json.Decode.succeed Measurement
--        |> Json.Decode.Pipeline.required "measurement" string


decoderVariable : Decoder Variable
decoderVariable =
    Json.Decode.succeed Variable
        |> Json.Decode.Pipeline.required "variable" string
        |> Json.Decode.Pipeline.optional "description" string ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
