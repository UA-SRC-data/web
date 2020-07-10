module Page.Scrutinizer exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Config exposing (apiServer)
import Html exposing (Html, div, h1, h2, input, p, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import HttpBuilder
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Maybe.Extra
import RemoteData exposing (RemoteData, WebData)
import Table exposing (defaultCustomizations)
import Url.Builder


type alias Model =
    { records : WebData (List Record)
    , query : String
    , tableState : Table.State
    , variables : WebData (List Variable)
    , minValue : Maybe Float
    , maxValue : Maybe Float
    , locationType : Maybe String
    , selectedVariable : Maybe String
    }


type alias Variable =
    { variable : String
    , description : String
    }


type alias Record =
    { id : String
    , variable_name : String
    , variable_desc : String
    , location_name : String
    , location_type : String
    , collected_on : String
    , medium : String
    , value : Float
    }


type Msg
    = MakeRequest
    | ClearSelections
    | DataResponse (WebData (List Record))
    | VariablesResponse (WebData (List Variable))
    | SelectVariable String
    | SetQuery String
    | SetTableState Table.State
    | SetMinValue String
    | SetMaxValue String
    | SetLocationType String


init : ( Model, Cmd Msg )
init =
    ( { records = RemoteData.NotAsked
      , tableState = Table.initialSort "value"
      , query = ""
      , minValue = Nothing
      , maxValue = Nothing
      , locationType = Nothing
      , variables = RemoteData.NotAsked
      , selectedVariable = Nothing
      }
    , Cmd.batch [ getVariables ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearSelections ->
            ( { model
                | records = RemoteData.NotAsked
                , selectedVariable = Nothing
                , minValue = Nothing
                , maxValue = Nothing
              }
            , Cmd.none
            )

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
            ( { model | maxValue = String.toFloat max }, Cmd.none )

        SetMinValue min ->
            ( { model | minValue = String.toFloat min }, Cmd.none )

        SetLocationType newType ->
            let
                newVal =
                    if String.length newType == 0 then
                        Nothing

                    else
                        Just newType
            in
            ( { model | locationType = newVal }, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )



-- view : Model -> Html Msg
-- view model =
--     div []
--         [ h2 [] [ text "Central Scrutinizer" ]
--         , viewForm model
--         , viewData model
--         ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.xs, Col.md4 ]
                [ h2 [] [ text "Central Scrutinizer" ]
                , viewForm model
                ]
            , Grid.col [ Col.md8 ] [ viewData model ]
            ]
        ]


viewData : Model -> Html Msg
viewData model =
    case model.records of
        RemoteData.NotAsked ->
            div [] [ text "" ]

        RemoteData.Loading ->
            div [] [ text "Loading data..." ]

        RemoteData.Failure httpError ->
            div [] [ text (viewHttpErrorMessage httpError) ]

        RemoteData.Success data ->
            div [ style "width" "100%", style "align" "center" ]
                [ text ("Found " ++ String.fromInt (List.length data))
                , Table.view tblConfig model.tableState data
                ]


viewForm : Model -> Html Msg
viewForm model =
    let
        selectedVar =
            Maybe.Extra.isJust model.selectedVariable

        selectedVal =
            List.any Maybe.Extra.isJust [ model.minValue, model.maxValue ]

        disableSearch =
            not (Bool.Extra.any [ selectedVar, selectedVal ])
    in
    Form.form []
        [ variableSelect model.variables
        , locationTypeSelect
        , countMinInput
        , countMaxInput
        , Button.button
            [ Button.onClick MakeRequest
            , Button.primary
            , Button.disabled disableSearch
            ]
            [ text "Search" ]

        --, Button.button [ Button.onClick ClearSelections, Button.secondary ]
        --    [ text "Reset" ]
        ]


countMinInput : Html Msg
countMinInput =
    Form.group []
        [ Form.label [ for "min_value" ] [ text "Min Value" ]
        , Input.number [ Input.onInput SetMinValue ]
        ]


countMaxInput : Html Msg
countMaxInput =
    Form.group []
        [ Form.label [ for "max_value" ] [ text "Max Value" ]
        , Input.number [ Input.onInput SetMaxValue ]
        ]


locationTypeSelect : Html Msg
locationTypeSelect =
    Form.group []
        [ Form.label [ for "location_type" ] [ text "Location Type" ]
        , Input.text [ Input.onInput SetLocationType ]
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
                    case String.length item.description of
                        0 ->
                            item.variable

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

        maybeToFloat m =
            case m of
                Nothing ->
                    ""

                Just n ->
                    String.fromFloat n

        builder ( label, value ) =
            case value of
                Just v ->
                    Just (Url.Builder.string label v)

                _ ->
                    Nothing

        queryParams =
            Url.Builder.toQuery <|
                List.filterMap builder
                    [ ( "variable", model.selectedVariable )
                    , ( "min_value", Maybe.map String.fromFloat model.minValue )
                    , ( "max_value", Maybe.map String.fromFloat model.maxValue )
                    , ( "location_type", model.locationType )
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
    Table.customConfig
        { columns =
            [ Table.stringColumn "Variable" .variable_name
            , Table.stringColumn "Desc" .variable_desc
            , Table.stringColumn "Location" .location_name
            , Table.stringColumn "Location Type" .location_type
            , Table.stringColumn "Medium" .medium
            , Table.stringColumn "Collected" .collected_on
            , Table.floatColumn "Value" .value
            ]
        , toId = .id
        , toMsg = SetTableState
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ class "table table-sm table-striped"
                    , style "font-size" "0.85em"
                    ]
            }
        }


decoderData : Decoder Record
decoderData =
    Json.Decode.succeed Record
        |> Json.Decode.Pipeline.required "id" string
        |> Json.Decode.Pipeline.required "variable_name" string
        |> Json.Decode.Pipeline.optional "variable_desc" string ""
        |> Json.Decode.Pipeline.required "location_name" string
        |> Json.Decode.Pipeline.required "location_type" string
        |> Json.Decode.Pipeline.required "collected_on" string
        |> Json.Decode.Pipeline.required "medium" string
        |> Json.Decode.Pipeline.required "value" float


decoderVariable : Decoder Variable
decoderVariable =
    Json.Decode.succeed Variable
        |> Json.Decode.Pipeline.required "name" string
        |> Json.Decode.Pipeline.optional "desc" string ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
