library(testthat)
library(tsForecastR)

context("Test individual forecasting procedures")

test_that("generate_fc_arima_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_arima(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_arima(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_stl_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_stl(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_stl(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_ets_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_ets(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_ets(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_nnetar_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_nnetar(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_nnetar(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_snaive_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_snaive(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_snaive(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_bsts_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_bsts(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_bsts(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
test_that("generate_fc_lstm_keras_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  model_name <- check_tensorflow("lstm_keras")
  if (base::length(model_name) > 0) {
    fc <- generate_fc_lstm_keras(ts_data)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_lstm_keras(xts_data)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), "period_1")
  } else {
    warning("LSTM model could not be tested! To use the LSTM model, Python and Tensorflow (version <= 1.14) must be installed.")
  }
})
test_that("generate_fc_automl_h2o_works", {
  data <- seq(1:144)
  ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
  fc <- generate_fc_automl_h2o(ts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")

  xts_data <- xts::as.xts(ts_data)
  fc <- generate_fc_automl_h2o(xts_data, model_names = model_names)
  expect_equal(class(fc)[1], "tsForecastR")
  expect_equal(names(fc), "period_1")
})
