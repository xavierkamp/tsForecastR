[![Travis build status](https://travis-ci.org/xavierkamp/tsForecastR.svg?branch=master)](https://travis-ci.org/xavierkamp/tsForecastR)
[![Codecov test coverage](https://codecov.io/gh/xavierkamp/tsForecastR/branch/master/graph/badge.svg)](https://codecov.io/gh/xavierkamp/tsForecastR?branch=master)

# __Time Series Forecasting__
This project involves developing a forecasting engine which is capable of applying traditional time series and 
machine learning models on univariate and multivariate time series. 
The idea is to facilitate forecasting accuracy testing of these methods through backtesting approaches. The procedure is meant to handle different case studies such as demand forecasting, inventory projections and financial forecasting. As the forecasting procedure is based on 'xts' data types, it can handle a variety of data frequencies (e.g. yearly, monthly, daily).

All codes are written in R.

## __Acknowledgements__
The main source of inspiration of this project is the TSstudio package which presents excellent powerful tools for dealing with time series data. Nevertheless, at the time of this writing (24/01/2020), TSstudio does not provide a framework to include newer types of machine learning algorithms such as AutoML from the h2o package and Long Short-term Memory Networks. This led to the need for a framework which would enable a possible extension to these newer models which this package hopes to provide.

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

Note:

When installing this package, in order to use the LSTM model, you will be required to have Python and Tensorflow (version <= 1.14) installed on your machine. Note that the current forecasting procedure does not support Tensorflow 2.0.

To check the tensorflow version:
``` r
tensorflow::tf_config()
```
To install tensorflow:
``` r
tensorflow::install_tensorflow(version = 1.14)
```
When installing packages, you may be required to restart the R session and repeat the installation process in order to successfully install or update packages.

### Dependencies

List of the main dependencies of this package:

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

Optional dependency:

- keras

## __Functions__

List of main functions:

- generate_fc
- generate_fc_arima
- generate_fc_automl_h2o
- generate_fc_bsts
- generate_fc_ets
- generate_fc_lstm_keras (available if keras correctly installed)
- generate_fc_nnetar
- generate_fc_snaive
- generate_fc_stl
- generate_fc_tbats
- save_as_df

Note:

The above listed functions are all based on a sequential processing approach. To use parallel processing, please also inspect the [parTsForecastR](https://github.com/xavierkamp/parTsForecastR) package which is built on top of this package.

Example:
``` r
library(datasets)
library(tsForecastR)
# Generate forecasts on twelve periods
fc <- generate_fc(AirPassengers,
                  fc_horizon = 12)
df <- save_as_df(fc)
print(df)

# Generate forecasts on twelve most recent dates
fc <- generate_fc(AirPassengers,
                  model_names = "arima",
                  fc_horizon = 12,
                  backtesting_opt = list(use_bt = TRUE))
df <- save_as_df(fc)
print(df)
```
