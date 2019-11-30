# Time Series Forecasting

This project involves developing a forecasting engine which is capable of applying traditional time series and 
machine learning models on individual and multiple time series (with or without a hierarchical structure). 
The idea is to test and compare the forecasting accuracy of these methods through backtesting and identify 
the best performing models. The  procedure is meant to handle different business cases such as demand forecasting, 
inventory projections, financial forecasting, etc. All codes are written in R.

## Table of contents
* [Getting Started](#getting-started)
* [Functions](#functions)
* [Acknowledgments](#acknowledgments)

## Getting Started

### Prerequisites

Install R: https://cloud.r-project.org/

Install RStudio: https://rstudio.com/products/rstudio/download/

### Dependencies

    bsts,
    doParallel,
    dplyr,
    foreach,
    forecast,
    h2o,
    keras,
    lubridate,
    parallel,
    purrr,
    stats,
    stringr,
    tibble,
    timetk,
    utils,
    xts,
    zoo

### Install

```
library("devtools")
devtools::install_github("xavierkamp/tsForecast")
```


## Functions


## Acknowledgments
