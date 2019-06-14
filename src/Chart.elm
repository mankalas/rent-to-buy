module Chart exposing (view)

import Chartjs.Chart as Chart exposing (..)
import Chartjs.Common exposing (..)
import Chartjs.Data exposing (..)
import Chartjs.DataSets.Line as L exposing (..)
import Chartjs.Options exposing (..)
import Chartjs.Options.Legend exposing (..)
import Chartjs.Options.Title exposing (..)
import Color


view =
    let
        chartDataset : L.DataSet
        chartDataset =
            let
                d =
                    L.defaultFromLabel "Bite"

                c =
                    Just (All (Color.rgb 1 0 0))
            in
            { d
                | data = [ 1, 2, 3 ]
                , backgroundColor = c
                , fill = Just L.Disabled
                , borderColor = c
            }

        chartDataset2 =
            let
                d =
                    L.defaultFromLabel "Bite"

                c =
                    Just (All (Color.rgb 0 1 0))
            in
            { d
                | data = [ 3, 2, 1 ]
                , backgroundColor = c
                , fill = Just L.Disabled
                , borderColor = c
            }

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
            { labels = [ "2019", "2020", "2021" ]
            , datasets =
                [ LineDataSet chartDataset
                , LineDataSet chartDataset2
                ]
            }

        finalChart : Chart
        finalChart =
            { chartType = Chart.Line
            , data = data
            , options = defaultOptions
            }
    in
    chart 95 95 finalChart
