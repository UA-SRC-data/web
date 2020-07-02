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
    , variables : WebData (List Variable)
    , locations : WebData (List Location)
    , tableState : Table.State
    , query : String
    , selectedVariable : Maybe String
    , selectedLocation : Maybe String
    , minValue : Maybe Float
    , maxValue : Maybe Float
    }


type alias Variable =
    { variable_name : String
    }


type alias Location =
    { location_name : String
    }


type alias Record =
    { collected_on : String
    , variable_name : String
    , location_name : String
    , value : Float
    }


type Msg
    = MakeRequest
    | DataResponse (WebData (List Record))
    | VariablesResponse (WebData (List Variable))
    | LocationsResponse (WebData (List Location))
    | SetQuery String
    | SetTableState Table.State
    | SelectVariable String
    | SelectLocation String
    | SetMinValue String
    | SetMaxValue String


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.NotAsked
      , variables = RemoteData.NotAsked
      , locations = RemoteData.NotAsked
      , tableState = Table.initialSort "Year"
      , query = ""
      , selectedVariable = Nothing
      , selectedLocation = Nothing
      , minValue = Nothing
      , maxValue = Nothing
      }
    , Cmd.batch
        [ getVariables, getLocations ]
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

        VariablesResponse data ->
            ( { model | variables = data }
            , Cmd.none
            )

        LocationsResponse data ->
            ( { model | locations = data }
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

        SelectLocation location ->
            let
                newLocation =
                    case location of
                        "" ->
                            Nothing

                        _ ->
                            Just location
            in
            ( { model | selectedLocation = newLocation }, Cmd.none )

        SetMaxValue max ->
            ( { model | maxValue = String.toFloat max }, Cmd.none )

        SetMinValue min ->
            ( { model | minValue = String.toFloat min }, Cmd.none )

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
                        [ td [] [ text rec.collected_on ]
                        , td [] [ text rec.variable_name ]
                        , td [] [ text rec.location_name ]
                        , td [ style "text-align" "right" ]
                            [ text (String.fromFloat rec.val) ]
                        ]

                header =
                    tr []
                        [ th [] [ text "Collected" ]
                        , th [] [ text "Variable" ]
                        , th [] [ text "Location" ]
                        , th [] [ text "Value" ]
                        ]

                lowerQuery =
                    String.toLower model.query

                filtered =
                    List.filter
                        (String.contains lowerQuery
                            << String.toLower
                            << .variable_name
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
        [ variablesSelect model.variables
        , locationsSelect model.locations
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


variablesSelect : WebData (List Variable) -> Html Msg
variablesSelect variables =
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
                    Variable ""

                makeItem item =
                    Select.item
                        [ value item.variable_name ]
                        [ text item.variable_name ]
            in
            Form.group []
                [ Form.label [ for "variable" ] [ text "Variable" ]
                , Select.select
                    [ Select.id "variables"
                    , Select.onChange SelectVariable
                    ]
                    (List.map makeItem ([ empty ] ++ data))
                ]


locationsSelect : WebData (List Location) -> Html Msg
locationsSelect locations =
    case locations of
        RemoteData.NotAsked ->
            div [] [ text "Not asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Failure httpError ->
            div [] [ text (viewHttpErrorMessage httpError) ]

        RemoteData.Success data ->
            let
                empty =
                    Location ""

                makeItem item =
                    Select.item
                        [ value item.location_name ]
                        [ text item.location_name ]
            in
            Form.group []
                [ Form.label [ for "location" ] [ text "Location" ]
                , Select.select
                    [ Select.id "locations", Select.onChange SelectLocation ]
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

        builder ( label, value ) =
            case value of
                Just v ->
                    Just (Url.Builder.string label v)

                _ ->
                    Nothing

        queryParams =
            Url.Builder.toQuery <|
                List.filterMap builder
                    [ ( "variable_name", model.selectedVariable )
                    , ( "location_name", model.selectedLocation )
                    , ( "val_min", Maybe.map String.fromFloat model.minValue )
                    , ( "val_max", Maybe.map String.fromFloat model.maxValue )
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


getVariables : Cmd Msg
getVariables =
    let
        url =
            apiServer ++ "/data/csm/variables"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> VariablesResponse)
                (Json.Decode.list decoderVariable)
        }


getLocations : Cmd Msg
getLocations =
    let
        url =
            apiServer ++ "/data/csm/locations"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> LocationsResponse)
                (Json.Decode.list decoderLocation)
        }


tblConfig : Table.Config Record Msg
tblConfig =
    Table.config
        { toId = .location_name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Collected" .collected_on
            , Table.stringColumn "Variable" .variable_name
            , Table.stringColumn "Location" .location_name
            , Table.floatColumn "Value" .value
            ]
        }


decoderData : Decoder Record
decoderData =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "collected_on" string
        |> Json.Decode.Pipeline.required "variable_name" string
        |> Json.Decode.Pipeline.required "location_name" string
        |> Json.Decode.Pipeline.required "value" float


decoderVariable : Decoder Variable
decoderVariable =
    Json.Decode.succeed Variable
        |> Json.Decode.Pipeline.required "variable_name" string


decoderLocation : Decoder Location
decoderLocation =
    Json.Decode.succeed Location
        |> Json.Decode.Pipeline.required "location_name" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
