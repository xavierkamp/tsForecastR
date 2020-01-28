[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.com/xavierkamp/tsForecastR.svg?branch=master)](https://travis-ci.com/xavierkamp/tsForecastR)
[![Codecov test coverage](https://codecov.io/gh/xavierkamp/tsForecastR/branch/master/graph/badge.svg)](https://codecov.io/gh/xavierkamp/tsForecastR?branch=master)

# __Time Series Forecasting__
This project involves developing a forecasting engine which can apply traditional time series models (e.g. ARIMA, ETS, STL) and machine learning models (e.g. AutoML-h2o, LSTM-keras) on univariate and multivariate time series. 
The idea is to facilitate forecast accuracy testing of these methods using different backtesting approaches. The forecasting procedures are designed to handle different case studies such as demand forecasting, inventory projections and possibly financial forecasting. As the procedures are based on 'xts' data types, a variety of data frequencies (e.g. yearly, monthly, daily) can be tested.

All codes are written in R.

## __Acknowledgements__
The main source of inspiration for this project is the TSstudio package which offers excellent powerful tools for dealing with time series data. However, at the time of this writing (January 2020), TSstudio does not provide a solution for testing newer types of machine learning algorithms such as the AutoML method from h2o or Long Short-term Memory Networks with keras. This project hopes to provide a possible solution to this interesting challenge and enable simple performance comparisons between traditional and machine learning models.

## __Getting Started__

### __Prerequisites__

Install R: https://cloud.r-project.org/

Install RStudio: https://rstudio.com/products/rstudio/download/

For Windows, also install Rtools: https://cran.r-project.org/bin/windows/Rtools/

### __Install__

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("xavierkamp/tsForecastR")
```

__Note:__

When installing this package, in order to use the LSTM model you will be required to have Python and Tensorflow (version <= 1.14) installed on your machine. Please note that currently the forecasting procedure does not support Tensorflow 2.0.

To check the tensorflow version:
``` r
tensorflow::tf_config()
```
To install tensorflow:
``` r
install.packages("tensorflow")
tensorflow::install_tensorflow(version = 1.14)
```
When installing packages, in order to successfully install or update packages, you may be required to restart the R session and repeat the installation process.

### __Dependencies__

List of main dependencies:

- bsts
- doParallel
- dplyr
- foreach
- forecast
- h2o
- imputeTS
- lubridate
- magrittr
- parallel
- plotly
- purrr
- stats
- stringr
- tibble
- timeSeries
- timetk
- utils
- xts
- zoo

Optional dependencies:

- keras
- tensorflow
- testthat
- covr

## __Functions__

__Part 1: Statistical Models__

__generate_fc__ - Function which enables the user to select different forecasting models from traditional time series models (e.g. ARIMA, ETS, STL) to machine learning algorithms (e.g. LSTM, AutoML). Results can be stored either as a tsForecastR object (default) or in text files.

Example:

``` r
library(datasets)
library(tsForecastR)

fc <- generate_fc(AirPassengers, fc_horizon = 12,
                  model_names = c("ets", "snaive",
                                  "stl", "nnetar",
                                  "lstm_keras",
                                  "automl_h2o"))
```
For more details:

``` r
help(generate_fc)
?generate_fc
```

Similar functions:

- generate_fc_arima
- generate_fc_automl_h2o
- generate_fc_bsts
- generate_fc_ets
- generate_fc_lstm_keras - (available only if tensorflow requirements are met)
- generate_fc_nnetar
- generate_fc_snaive
- generate_fc_stl
- generate_fc_tbats

__Part 2: Analysis__

__save_as_df__ - Function which transforms a tsForecastR object or selected text files into a data.frame object which can then be integrated in user-defined analyses.

Example:
``` r
library(datasets)
library(tsForecastR)
# Generate forecasts on twelve periods
fc <- generate_fc(AirPassengers,
                  fc_horizon = 12)
df <- save_as_df(fc)
print(df)
```

__Note:__

The above listed functions are all based on a sequential processing approach. To enable parallel processing, please refer to the [parTsForecastR](https://github.com/xavierkamp/parTsForecastR) package which is built on top of this package.
