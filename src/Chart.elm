module Chart exposing (Model, view)

import Chartjs.Chart as Chart exposing (..)
import Chartjs.Common exposing (..)
import Chartjs.Data exposing (..)
import Chartjs.DataSets.Line as L exposing (..)
import Chartjs.Options exposing (..)
import Chartjs.Options.Legend exposing (..)
import Chartjs.Options.Title exposing (..)
import Color


type alias Model =
    { hv : Float
    , hr : Float
    , lt : Int
    }


houseData : Model -> List Float
houseData m =
    let
        market_update : Int -> Float -> Float
        market_update n v =
            v * ((1 + (m.hr / 100)) ^ toFloat n)
    in
    List.indexedMap market_update (List.repeat m.lt m.hv)


houseValueDataSet : Model -> L.DataSet
houseValueDataSet m =
    let
        default =
            L.defaultFromLabel <| String.fromFloat m.hv

        color =
            Just (All (Color.rgb 1 0 0))
    in
    { default
        | data = houseData m
        , backgroundColor = color
        , fill = Just L.Disabled
        , borderColor = color
    }


view m =
    let
        title : Title
        title =
            defaultTitle

        options : Options
        options =
            { animations = Nothing
            , layout = Nothing
            , legend = Nothing
            , title = Just title
            , tooltips = Nothing
            , elements = Nothing
            , scales = Nothing
            , maintainAspectRatio = Nothing
            }

        data : Data
        data =
            { labels = List.indexedMap (\n y -> String.fromInt (y + n)) (List.repeat m.lt 2019)
            , datasets =
                [ LineDataSet (houseValueDataSet m)
                ]
            }

        finalChart : Chart
        finalChart =
            { chartType = Chart.Line
            , data = data
            , options = defaultOptions
            }
    in
    Debug.log "Chart" (chart 95 95 finalChart)
