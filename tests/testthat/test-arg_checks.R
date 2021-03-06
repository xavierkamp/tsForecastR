library(testthat)
library(tsForecastR)

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
test_that("check_data_dir_works", {
  expect_equal(check_data_dir(getwd()), getwd())
  expect_equal(check_data_dir(NULL), NULL)
  expect_error(check_data_dir("This_Is_not_a_v@lid_directory"))
  expect_error(check_data_dir(list("This_Is_not_a_v@lid_directory")))
})
test_that("check_model_names_works", {
  available_models <-
    c("arima", "ets", "tbats", "bsts",
      "stl", "snaive", "nnetar", "automl_h2o",
      "lstm_keras")
  expect_equal(check_model_names("arima"), "arima")
  valid_models <- check_tensorflow(available_models)
  expect_equal(check_model_names(NULL), valid_models)
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
  expect_equal(check_preprocess_fct(NULL), default_prepro_fct)
  expect_equal(check_preprocess_fct(imputeTS::na.mean), imputeTS::na.mean)
  expect_equal(check_preprocess_fct("sum"), default_prepro_fct)
  expect_equal(check_preprocess_fct(base::list()), default_prepro_fct)
  expect_equal(check_preprocess_fct(base::list(fct = base::sum, xreg = seq(1:10))),
               base::list(fct = base::sum, xreg = seq(1:10)))
})
test_that("check_time_id_works", {
  expect_equal(base::is.null(check_time_id(NULL)), TRUE)
  expect_equal(base::is.null(check_time_id("2010-10-10")), TRUE)
  expect_equal(base::is.null(check_time_id(base::list())), TRUE)
  time_id <- base::Sys.time()
  expect_equal(check_time_id(time_id), time_id)
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
