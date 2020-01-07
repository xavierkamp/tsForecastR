if (require(testthat)) {
  context("Test check functions")
  test_that("check_backtest_opt_works", {
    default_bt_opt <- base::list(use_bt = FALSE,
                                 nb_iters = 1,
                                 method = c("rolling",
                                            "moving"),
                                 sample_size = c("expanding",
                                                 "fixed"))
    bt_opt <- NULL
    expect_equal(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- base::list(use_bt = FALSE, nb_iters = 1)
    expect_equal(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- base::list(use_bt = FALSE, nb_iters = 3)
    expect_equivalent(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- base::list(use_bt = TRUE, nb_iters = 3)
    expect_gt(check_backtesting_opt(bt_opt)[["nb_iters"]],
                     default_bt_opt[["nb_iters"]])
    bt_opt <- base::list()
    expect_equivalent(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- FALSE
    expect_equivalent(check_backtesting_opt(bt_opt), default_bt_opt)
  })
  test_that("check_fc_horizon_works", {
    expect_error(check_fc_horizon("2"))
    expect_error(check_fc_horizon(TRUE))
    expect_error(check_fc_horizon(list()))
    expect_error(check_fc_horizon(-1))
    expect_error(check_fc_horizon(0))
    expect_equal(check_fc_horizon(4), 4)
  })
  test_that("check_valid_set_size_works", {
    expect_error(check_valid_set_size("2"))
    expect_error(check_valid_set_size(TRUE))
    expect_error(check_valid_set_size(list()))
    expect_error(check_valid_set_size(-1))
    expect_equal(check_valid_set_size(0), 0)
    expect_equal(check_valid_set_size(4), 4)
  })
  test_that("check_tmp_test_set_size_works", {
    expect_error(check_tmp_test_set_size("2"))
    expect_error(check_tmp_test_set_size(TRUE))
    expect_error(check_tmp_test_set_size(list()))
    expect_error(check_tmp_test_set_size(-1))
    expect_equal(check_tmp_test_set_size(0), 0)
    expect_equal(check_tmp_test_set_size(4), 4)
  })
  test_that("check_backtesting_iter_works", {
    expect_error(check_backtesting_iter("2"))
    expect_error(check_backtesting_iter(TRUE))
    expect_error(check_backtesting_iter(list()))
    expect_error(check_backtesting_iter(-1))
    expect_error(check_backtesting_iter(0))
    expect_equal(check_backtesting_iter(1, NULL), 1)
    expect_equal(check_backtesting_iter(1, backtesting_opt = list(nb_iters = 1)), 1)
    expect_error(check_backtesting_iter(3, backtesting_opt = list(nb_iters = 5)))
    expect_equal(check_backtesting_iter(3, backtesting_opt = list(use_bt = TRUE,
                                                                  nb_iters = 5)), 3)
    expect_error(check_backtesting_iter(5, backtesting_opt = list(use_bt = TRUE,
                                                                  nb_iters = 4)))
  })
  test_that("check_data_sv_as_xts_works", {
    expect_error(check_data_sv_as_xts("2"))
    expect_equal(check_data_sv_as_xts(NULL), NULL)
    data <- seq(1:10)
    expect_error(check_data_sv_as_xts(data))
    data_df <- as.data.frame(data)
    expect_error(check_data_sv_as_xts(data_df))
    data_ts <- ts(data, frequency = 1)
    expect_equal(xts::is.xts(check_data_sv_as_xts(data_ts)), TRUE)
    data_mts <- cbind(data_ts, data_ts)
    expect_equal(xts::is.xts(check_data_sv_as_xts(data_mts)), TRUE)
    data_xts <- xts::as.xts(data_ts)
    expect_equivalent(check_data_sv_as_xts(data_xts), data_xts)
    expect_equivalent(colnames(check_data_sv_as_xts(data_xts)), "time_series_1")
    colnames(data_xts) <- "ts.testN@me"
    expect_equivalent(colnames(check_data_sv_as_xts(data_xts)), "tstestNme")
  })
  test_that("check_save_fc_to_file_works", {
    expect_equal(check_save_fc_to_file(getwd()), getwd())
    expect_equal(check_save_fc_to_file(NULL), NULL)
    expect_error(check_save_fc_to_file("This_Is_not_a_v@lid_directory"))
    expect_error(check_save_fc_to_file(list("This_Is_not_a_v@lid_directory")))
  })
  test_that("check_model_names_works", {
    available_models <-
      c("arima", "ets", "tbats", "bsts",
        "stl", "snaive", "nnetar", "automl_h2o",
        "lstm_keras", "hybrid")
    expect_equal(check_model_names("arima"), "arima")
    expect_equal(check_model_names(list("arima", "ets")), c("arima", "ets"))
    expect_error(check_model_names("arfima"))
    expect_error(check_model_names(c("arima", "arfima")))
  })
  test_that("check_models_args_works", {
    model_names = NULL
    model_args = base::list()
    expect_identical(check_models_args(model_args, model_names), base::list())
    model_args = NULL
    expect_identical(check_models_args(model_args, model_names), base::list())
    model_args = "arima_arg"
    expect_error(check_models_args(model_args, model_names))
    model_args = base::list(arima_arg = NULL)
    expect_identical(check_models_args(model_args, model_names), model_args)
    model_args = base::list("arima_arg")
    expect_identical(check_models_args(model_args, model_names), base::list())
    model_args = base::list(arima_arg = base::list())
    expect_identical(check_models_args(model_args, model_names), model_args)
    model_args = base::list(arima_arg = base::list(P = 1), bsts = NULL)
    expect_identical(check_models_args(model_args, model_names),
                     base::list(arima_arg = base::list(P = 1)))
    model_args = base::list(arima_arg = base::list(P = 1), bsts_arg = NULL,
                            arlima_arg = NULL)
    expect_identical(check_models_args(model_args, model_names),
                     base::list(arima_arg = base::list(P = 1), bsts_arg = NULL))
    model_args = base::list(arima_arg = NULL, arima_arg = NULL)
    expect_identical(check_models_args(model_args, model_names), model_args)
  })
  test_that("check_nb_cores_works", {
    expect_equal(check_nb_cores(1), 1)
    nb_cores_available <- parallel::detectCores()
    expect_equal(check_nb_cores(nb_cores_available), nb_cores_available)
    expect_equal(check_nb_cores(nb_cores_available + 1), nb_cores_available)
    expect_equal(check_nb_cores("1"), nb_cores_available)
    expect_equal(check_nb_cores(base::list()), nb_cores_available)
  })
  test_that("check_preprocessing_fct_works", {
    expect_equal(check_preprocess_fct(NULL), NULL)
    expect_equal(check_preprocess_fct(imputeTS::na.mean), imputeTS::na.mean)
    expect_equal(check_preprocess_fct("sum"), NULL)
    expect_equal(check_preprocess_fct(base::list()), NULL)
    expect_equal(check_preprocess_fct(base::list(fct = base::sum, xreg = seq(1:10))),
                 base::list(fct = base::sum, xreg = seq(1:10)))
  })
  test_that("check_time_id_works", {
    expect_equal(class(check_time_id(NULL))[1], "POSIXct")
    expect_equal(class(check_time_id("2010-10-10"))[1], "POSIXct")
    expect_equal(class(check_time_id(base::list()))[1], "POSIXct")
    expect_equal(class(check_time_id(as.character(base::Sys.time())))[1], "POSIXct")
    unique_time <- base::Sys.time()
    expect_equal(check_time_id(unique_time), unique_time)
  })
  test_that("check_period_iter_works", {
    expect_error(check_period_iter("1"))
    expect_error(check_period_iter("priod_1"))
    expect_error(check_period_iter(base::list("period_1")))
    expect_error(check_period_iter(base::list()))
    expect_error(check_period_iter("period_per_1"))
    expect_error(check_period_iter("period__1"))
    expect_error(check_period_iter("1_this.Is@_test_%period_1"))
    expect_error(check_period_iter("1thisIs_test_period_1"))
    expect_error(check_period_iter("Period_1"))
    expect_error(check_period_iter("period1"))
    expect_equal(check_period_iter("period_1"), "period_1")
    expect_equal(check_period_iter("period_period_1"), "period_1")
  })

  context("Test fc generating functions")
  test_that("generate_fc_works", {
    data <- seq(1:144)
    model_names <- c("arima", "ets", "snaive", "bsts", "nnetar", "stl")

    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "time_series_1")

    mts_data <- base::cbind(ts_data, 2*ts_data)
    colnames(mts_data) <- NULL
    fc <- generate_fc(mts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), c("time_series_1", "time_series_2"))

    xts_data <- xts::as.xts(mts_data)
    fc <- generate_fc(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), c("time_series_1", "time_series_2"))
  })
  test_that("generate_fc_arima_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_arima(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "time_series_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_arima(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_stl_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_stl(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_stl(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_ets_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_ets(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_ets(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_nnetar_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_nnetar(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_nnetar(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_snaive_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_snaive(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_snaive(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_bsts_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_bsts(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_bsts(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_lstm_keras_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_lstm_keras(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_lstm_keras(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
  test_that("generate_fc_automl_h2o_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_automl_h2o(ts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")

    xts_data <- xts::as.xts(ts_data)
    fc <- generate_fc_automl_h2o(xts_data, model_names = model_names)
    expect_equal(is.list(fc), TRUE)
    expect_equal(names(fc), "period_1")
  })
}
