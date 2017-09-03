module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onCheck)
import Ecdf exposing (ecdf, confidenceBandLower, confidenceBandUpper)
import Charty.LineChart as LineChart
import Html.Attributes as Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.CDN as CDN


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Domain =
    { min : Float
    , max : Float
    }


type alias Model =
    { values : List Float
    , ecdfEstimate : List Float
    , ecdfCILower : List Float
    , ecdfCIUpper : List Float
    , inputOk : Bool
    , domain : Domain
    , input : String
    , alpha : Result String Float
    , alphaOk : Bool
    }


model : Model
model =
    let
        alpha =
            0.05

        values : List Float
        values =
            [ 171.88
            , 63.01
            , 41.27
            , 159.75
            , 84.11
            , 177.03
            , 118.87
            , 89.9
            , 127.22
            , 24.69
            , 122.23
            , 139.42
            , 21.51
            , 208.21
            , 18.79
            ]

        n : Int
        n =
            List.length values

        ecdfValues : List Float
        ecdfValues =
            expandDomain domain
                |> List.map (\x -> ecdf x values)
                |> List.map (Maybe.withDefault 0)

        ecdfCILower : List Float
        ecdfCILower =
            lowerCI alpha n ecdfValues

        ecdfCIUpper : List Float
        ecdfCIUpper =
            upperCI alpha n ecdfValues

        domain =
            newDomain values
    in
        { values = values
        , domain = domain
        , ecdfEstimate = ecdfValues
        , ecdfCILower = ecdfCILower
        , ecdfCIUpper = ecdfCIUpper
        , inputOk = True
        , input = encodeValues values
        , alpha = Ok alpha
        , alphaOk = True
        }



-- UPDATE


type Msg
    = DatasetChange String
    | AlphaChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AlphaChange text ->
            case String.toFloat text of
                Ok alpha ->
                    if alpha > 0 && alpha < 1 then
                        let
                            n =
                                List.length model.values

                            ecdfCILower : List Float
                            ecdfCILower =
                                lowerCI (alpha / 2) n model.ecdfEstimate

                            ecdfCIUpper : List Float
                            ecdfCIUpper =
                                upperCI (alpha / 2) n model.ecdfEstimate
                        in
                            { model
                                | alpha = Ok alpha
                                , ecdfCILower = ecdfCILower
                                , ecdfCIUpper = ecdfCIUpper
                                , alphaOk = True
                            }
                    else
                        { model | alpha = Err text, alphaOk = False }

                Err msg ->
                    { model | alpha = Err text, alphaOk = False }

        DatasetChange text ->
            case Decode.decodeString decoder text of
                Result.Ok values ->
                    let
                        domain =
                            newDomain values

                        n : Int
                        n =
                            List.length values

                        ecdfEstimate : List Float
                        ecdfEstimate =
                            expandDomain domain
                                |> List.map (\x -> ecdf x values)
                                |> List.map (Maybe.withDefault 0)

                        alpha =
                            case model.alpha of
                                Ok val ->
                                    val / 2

                                Err val ->
                                    0.05 / 2

                        ecdfCILower : List Float
                        ecdfCILower =
                            lowerCI alpha n ecdfEstimate

                        ecdfCIUpper : List Float
                        ecdfCIUpper =
                            upperCI alpha n ecdfEstimate
                    in
                        { model
                            | values = values
                            , domain = domain
                            , ecdfEstimate = ecdfEstimate
                            , ecdfCILower = ecdfCILower
                            , ecdfCIUpper = ecdfCIUpper
                            , inputOk = True
                        }

                Result.Err err ->
                    { model | inputOk = False }



-- VIEW


view : Model -> Html Msg
view model =
    let
        defaults =
            LineChart.defaults

        chart =
            LineChart.view
                { defaults
                    | drawPoints = False
                    , drawLabels = True
                }
                (dataset model)

        opacity =
            if model.inputOk && model.alphaOk then
                1
            else
                0.3

        alpha =
            case model.alpha of
                Ok val ->
                    toString val

                Err val ->
                    val

        mainContent model =
            Grid.container []
                [ Html.blockquote []
                    [ text "In statistics, an empirical distribution function is the distribution function associated with the empirical measure of a sample. This cumulative distribution function is a step function that jumps up by 1/n at each of the n data points. Its value at any specified value of the measured variable is the fraction of observations of the measured variable that are less than or equal to the specified value."
                    , Html.a [ Attributes.href "https://en.wikipedia.org/wiki/Empirical_distribution_function" ] [ text "Wikipedia" ]
                    ]
                , Grid.row
                    []
                    [ Grid.col [ Col.xs3 ]
                        [ Html.div
                            []
                            [ Html.div [ Attributes.class "form-group" ]
                                [ Html.label [] [ text "Data" ]
                                , Html.textarea
                                    [ Attributes.class "form-control"
                                    , Attributes.style [ ( "height", "50vh" ) ]
                                    , Html.Events.onInput DatasetChange
                                    ]
                                    [ Html.text model.input ]
                                , Html.small [ Attributes.class "form-text text-muted" ]
                                    [ text "Feel free to edit the numbers above directly. Just ensure it is a valid JSON array: i.e. your numbers are comma separated surrounded by square brackets."
                                    ]
                                ]
                            , Html.div
                                [ Attributes.class "form-group" ]
                                [ Html.label [] [ text "Significance level" ]
                                , Html.input
                                    [ Html.Events.onInput AlphaChange
                                    , Attributes.value alpha
                                    , Attributes.class "form-control"
                                    , Attributes.type_ "number"
                                    , Attributes.step "0.01"
                                    , Attributes.min "0.01"
                                    , Attributes.max "0.99"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    , Grid.col [ Col.xs9 ]
                        [ (Html.div
                            [ Attributes.style [ ( "opacity", toString opacity ) ] ]
                            [ chart ]
                          )
                        ]
                    ]
                ]
    in
        Grid.container []
            [ CDN.stylesheet
            , Html.h1
                []
                [ text "Empirical distribution function" ]
            , mainContent model
            , Html.footer [ Attributes.class "blockquote-footer" ]
                [ Html.p []
                    [ text "Written in elm. Fork it on "
                    , Html.a
                        [ Attributes.href "https://github.com/dirkschumacher/elm-ecdf-spa" ]
                        [ text "Github" ]
                    , text "."
                    ]
                ]
            ]



-- HELPERS


newDomain : List Float -> Domain
newDomain values =
    { min = List.foldl min 0 values
    , max = List.foldl max 0 values
    }


expandDomain : Domain -> List Float
expandDomain domain =
    List.range (round domain.min) (round domain.max)
        |> List.map toFloat


encodeValues : List Float -> String
encodeValues values =
    Encode.encode 1 (Encode.list (List.map Encode.float values))


dataset : Model -> LineChart.Dataset
dataset model =
    let
        domain =
            expandDomain model.domain

        alphaText =
            case model.alpha of
                Ok val ->
                    (toString (toFloat (round ((1 - val) * 1000)) / 10)) ++ "%"

                Err text ->
                    ""
    in
        [ { label = "ecdf"
          , data = zip domain model.ecdfEstimate
          }
        , { label = "Lower " ++ alphaText ++ " CI limit "
          , data = zip domain model.ecdfCILower
          }
        , { label = "Upper " ++ alphaText ++ " CI limit "
          , data = zip domain model.ecdfCIUpper
          }
        ]


upperCI : Float -> Int -> List Float -> List Float
upperCI alpha n ecdfValues =
    List.map (\x -> confidenceBandUpper alpha n x) ecdfValues


lowerCI : Float -> Int -> List Float -> List Float
lowerCI alpha n ecdfValues =
    List.map (\x -> confidenceBandLower alpha n x) ecdfValues


zip : List Float -> List Float -> List ( Float, Float )
zip xs ys =
    List.map2 (,) xs ys


decoder : Decode.Decoder (List Float)
decoder =
    Decode.list Decode.float
