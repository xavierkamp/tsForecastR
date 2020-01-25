if (require(testthat)) {
  context("Test fc generating functions")
  test_that("generate_fc_works", {
    data <- seq(1:144)
    model_names <- c("arima", "ets", "snaive", "bsts", "nnetar", "stl")
    # check input data
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc(ts_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), "time_series_1")
    mts_data <- base::cbind(ts_data, 2*ts_data)
    colnames(mts_data) <- NULL
    fc <- generate_fc(mts_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), c("time_series_1", "time_series_2"))
    xts_data <- xts::as.xts(mts_data)
    fc <- generate_fc(xts_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), c("time_series_1", "time_series_2"))
    # check with missing data
    xts_na_data <-
      c(seq(1:144), rep(NA, 2), seq(1:4)) %>%
      stats::ts(., frequency = 12, start = c(1, 1)) %>%
      xts::as.xts()
    fc <- generate_fc(xts_na_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    # check preprocess function
    fc <- generate_fc(xts_na_data, model_names = model_names,
                      preprocess_fct = imputeTS::na_mean)
    expect_equal(class(fc)[1], "tsForecastR")
    fc <- generate_fc(xts_na_data, model_names = model_names,
                      preprocess_fct = "imputeTS::na_mean")
    expect_equal(class(fc)[1], "tsForecastR")
    # check backtesing opt
    fc1 <- generate_fc(ts_data, model_names = model_names,
                       backtesting_opt = TRUE)
    fc2 <- generate_fc(ts_data, model_names = model_names,
                       backtesting_opt = list())
    fc3 <- generate_fc(ts_data, model_names = model_names,
                       backtesting_opt = list(use_bt = TRUE))
    fc4 <- generate_fc(ts_data, model_names = model_names,
                       backtesting_opt = list(use_bt = TRUE,
                                              nb_iters = 1))
    # check nb cores
    model_names_2 <- c("arima", "automl_h2o")
    fc <- generate_fc(ts_data, model_names = model_names_2, nb_cores = 2)
    fc <- generate_fc(ts_data, model_names = model_names_2, nb_cores = 0)
    fc <- generate_fc(ts_data, model_names = model_names_2, nb_cores = "1")
    # check time id
    fc <- generate_fc(ts_data, model_names = model_names, time_id = "1234")
    # check model names
    model_names_2 <- c(model_names, "invalid_model")
    expect_error(generate_fc(ts_data, model_names = model_names_2))
    # check model args
    model_args <- list(arima_arg = NULL,
                       invalid_model_arg = c(P = 1))
    fc <- generate_fc(ts_data, model_names = model_names,
                      models_args = model_args)
    expect_equal(class(fc)[1], "tsForecastR")
    # check save_fc_to_file
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    tmp_dir <- base::paste(getwd(), "tmp_testing_dir", sep = "/")
    while (dir.exists(tmp_dir)) {
      tmp_dir <- base::paste(tmp_dir, "1", sep = "")
    }
    dir.create(tmp_dir)
    fc <- generate_fc(ts_data, model_names = model_names,
                      save_fc_to_file = tmp_dir)
    for (ts_name in base::names(fc)) {
      for (i in model_names) {
        file_name <- base::paste(tmp_dir,
                                 base::paste(ts_name,
                                             i,
                                             sep = "_"),
                                 sep = "/")
        expect_equal(file.exists(file_name), TRUE)
      }
    }
    expect_error(generate_fc(ts_data, model_names = model_names,
                             save_fc_to_file = "1234"))
    base::unlink(tmp_dir, recursive = TRUE)
  })
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
    fc <- generate_fc_lstm_keras(ts_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_lstm_keras(xts_data, model_names = model_names)
    expect_equal(class(fc)[1], "tsForecastR")
    expect_equal(names(fc), "period_1")
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
}
