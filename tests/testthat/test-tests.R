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
    expect_equivalent(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- base::list(use_bt = FALSE, nb_iters = 3)
    expect_equivalent(check_backtesting_opt(bt_opt), default_bt_opt)
    bt_opt <- base::list(use_bt = TRUE, nb_iters = 3)
    expect_more_than(check_backtesting_opt(bt_opt)[["nb_iters"]],
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
    data_xts <- xts::as.xts(data_valid)
    expect_equivalent(check_data_sv_as_xts(data_xts), data_xts)
    expect_equivalent(colnames(check_data_sv_as_xts(data_xts)), "time_series_1")
    colnames(data_xts) <- "ts.testN@me"
    expect_equivalent(colnames(check_data_sv_as_xts(data_xts)), "tstestNme")
  })
  test_that("check_save_fc_to_file_works", {
    expect_equal(check_save_fc_to_file("2"), "2")
    expect_equal(check_save_fc_to_file(NULL), NULL)
    current_wd <- getwd()
    expect_equal(check_save_fc_to_file(current_wd), current_wd)
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
}

