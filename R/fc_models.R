#' ARIMA Model
#' @description Function to apply the \code{\link[forecast]{auto.arima}} function from the \code{forecast} package
#' on time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param xreg_xts A univariate or multivariate ts, mts or xts object, optional external regressors
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param arima_arg A list, optional arguments to pass to the \code{\link[forecast]{auto.arima}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_arima(AirPassengers,
#'                         fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_arima(AirPassengers,
#'                         fc_horizon = 12,
#'                         backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_arima(AirPassengers,
#'                         fc_horizon = 6,
#'                         backtesting_opt = list(use_bt = TRUE,
#'                                                nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_arima <- function(ts_data_xts,
                              fc_horizon = 12,
                              xreg_xts = NULL,
                              backtesting_opt = NULL,
                              save_fc_to_file = NULL,
                              arima_arg = NULL,
                              ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  xreg_xts <- check_data_sv_as_xts(xreg_xts)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  if (is.null(arima_arg)) {
    arima_arg = base::list(D=1 , max.p = 2, max.q = 2,
                           max.P = 2,max.Q = 2,
                           max.d = 2,stationary = FALSE,
                           seasonal = TRUE,
                           stepwise = FALSE)
  } else if (!base::is.list(arima_arg)) {
    stop("Model arguments must be of type list!")
  }
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <-
    add_placeholders(ts_data_xts, fc_horizon, backtesting_opt) %>%
    add_features(xreg_xts)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         bt_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)

    x_train <- sample_split[["train"]][, base::colnames(ts_data_xts)]
    x_test <- sample_split[["test"]][, base::colnames(ts_data_xts)]
    if (!is.null(xreg_xts)) {
      xreg_names <- colnames(ts_contiguous_data)[!colnames(ts_contiguous_data) %in% colnames(ts_data_xts)]
      xreg_train <- sample_split[["train"]][, xreg_names]
      xreg_test <- sample_split[["test"]][, xreg_names]
      arima_arg$xreg <- xreg_train
    } else {
      xreg_test <- NULL
    }
    md <- base::do.call(forecast::auto.arima, c(base::list(x_train), arima_arg))
    fc <- forecast::forecast(md, h = fc_horizon, xreg = xreg_test)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Exponential Smoothing Model
#' @description Function to apply the \code{\link[forecast]{ets}} function from the \code{forecast} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param ets_arg A list, optional arguments to pass to the \code{\link[forecast]{ets}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_ets(AirPassengers,
#'                       fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_ets(AirPassengers,
#'                       fc_horizon = 12,
#'                       backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_ets(AirPassengers,
#'                       fc_horizon = 6,
#'                       backtesting_opt = list(use_bt = TRUE,
#'                                              nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_ets <- function(ts_data_xts,
                            fc_horizon = 12,
                            backtesting_opt = NULL,
                            save_fc_to_file = NULL,
                            ets_arg = NULL,
                            ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  if (base::is.null(ets_arg)) {
    ets_arg = base::list(model = "ZZA",
                         opt.crit = "amse",
                         upper = c(0.3, 0.2, 0.2, 0.98))
  } else if (!base::is.list(ets_arg)) {
    stop("Model arguments must be of type list!")
  }
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <- add_placeholders(ts_data_xts, fc_horizon, backtesting_opt)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         bt_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <- sample_split[["train"]]
    x_test <- sample_split[["test"]]
    md <- base::do.call(forecast::ets, c(base::list(x_train), ets_arg))
    fc <- forecast::forecast(md, h = fc_horizon)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' TBATS Model
#' @description Function to apply the \code{\link[forecast]{tbats}} function from the \code{forecast} package on
#' time series data. The \code{\link[forecast]{tbats}} function is only applicable on ts objects.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param tbats_arg A list, optional arguments to pass to the \code{\link[forecast]{tbats}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_tbats(AirPassengers,
#'                         fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_tbats(AirPassengers,
#'                         fc_horizon = 12,
#'                         backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_tbats(AirPassengers,
#'                         fc_horizon = 6,
#'                         backtesting_opt = list(use_bt = TRUE,
#'                                                nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_tbats <- function(ts_data_xts,
                              fc_horizon = 12,
                              backtesting_opt = NULL,
                              save_fc_to_file = NULL,
                              tbats_arg = NULL,
                              ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <- add_placeholders(ts_data_xts, fc_horizon, backtesting_opt)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <-
      sample_split[["train"]] %>%
      {
        dates <-
          zoo::index(.) %>%
          lubridate::as_date()
        data <-
          stats::ts(., start = c(lubridate::year(dates[1]),
                                 lubridate::month(dates[1])),
                    frequency = stats::frequency(.))
        data
      }
    x_test <-
      sample_split[["test"]] %>%
      {
        dates <-
          zoo::index(.) %>%
          lubridate::as_date()
        data <-
          stats::ts(., start = c(lubridate::year(dates[1]),
                                 lubridate::month(dates[1])),
                    frequency = stats::frequency(x_train))
        data
      }
    md <- base::do.call(forecast::tbats, c(base::list(x_train), tbats_arg))
    fc <- forecast::forecast(md, h = fc_horizon)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Neural Network
#' @description Function to apply the \code{\link[forecast]{nnetar}} function from the \code{forecast} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param xreg_xts A univariate or multivariate ts, mts or xts object, optional external regressors
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param nnetar_arg A list, optional arguments to pass to the \code{\link[forecast]{nnetar}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future periods
#' fc <- generate_fc_nnetar(AirPassengers,
#'                          fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_nnetar(AirPassengers,
#'                          fc_horizon = 12,
#'                          backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_nnetar(AirPassengers,
#'                          fc_horizon = 6,
#'                          backtesting_opt = list(use_bt = TRUE,
#'                                                 nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_nnetar <- function(ts_data_xts,
                               fc_horizon = 12,
                               xreg_xts = NULL,
                               backtesting_opt = NULL,
                               save_fc_to_file = NULL,
                               nnetar_arg = NULL,
                               ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  xreg_xts <- check_data_sv_as_xts(xreg_xts)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <-
    add_placeholders(ts_data_xts, fc_horizon, backtesting_opt) %>%
    add_features(xreg_xts)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <- sample_split[["train"]][, base::colnames(ts_data_xts)]
    x_test <- sample_split[["test"]][, base::colnames(ts_data_xts)]
    if (!base::is.null(xreg_xts)) {
      xreg_names <- base::colnames(ts_contiguous_data)[!base::colnames(ts_contiguous_data) %in% base::colnames(ts_data_xts)]
      xreg_train <- sample_split[["train"]][, xreg_names]
      xreg_test <- sample_split[["test"]][, xreg_names]
      nnetar_arg$reg <- xreg_train
    } else {
      xreg_test <- NULL
    }
    md <- base::do.call(forecast::nnetar, c(base::list(x_train), nnetar_arg))
    fc <- forecast::forecast(md, h = fc_horizon, xreg = xreg_test)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Season-Trend Decomposition with Loess Model
#' @description Function to apply the \code{\link[stats]{stl}} function from the \code{stats} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param stl_arg A list, optional arguments to pass to the \code{\link[stats]{stl}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_stl(AirPassengers,
#'                       fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_stl(AirPassengers,
#'                       fc_horizon = 12,
#'                       backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_stl(AirPassengers,
#'                       fc_horizon = 6,
#'                       backtesting_opt = list(use_bt = TRUE,
#'                                              nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_stl <- function(ts_data_xts,
                            fc_horizon = 12,
                            backtesting_opt = NULL,
                            save_fc_to_file = NULL,
                            stl_arg = NULL,
                            ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  if (base::is.null(stl_arg)) {
    stl_arg = base::list(s.window = "periodic")
  }
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <- add_placeholders(ts_data_xts, fc_horizon, backtesting_opt)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <- sample_split[["train"]]
    x_test <- sample_split[["test"]]
    md <- base::do.call(stats::stl, c(base::list(x_train), stl_arg))
    fc <- forecast::forecast(md, h = fc_horizon)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Seasonal Naive Model
#' @description Function to apply the \code{\link[forecast]{snaive}} function from the \code{forecast} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param snaive_arg A list, optional arguments to pass to the \code{\link[forecast]{snaive}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_snaive(AirPassengers,
#'                          fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_snaive(AirPassengers,
#'                          fc_horizon = 12,
#'                          backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_snaive(AirPassengers,
#'                          fc_horizon = 6,
#'                          backtesting_opt = list(use_bt = TRUE,
#'                                                 nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_snaive <- function(ts_data_xts,
                               fc_horizon = 12,
                               backtesting_opt = NULL,
                               save_fc_to_file = NULL,
                               snaive_arg = NULL,
                               ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_output <- base::list()
  md <- fc <- NULL
  ts_contiguous_data <- add_placeholders(ts_data_xts, fc_horizon, backtesting_opt)
  if (fc_horizon > 2 * stats::frequency(ts_contiguous_data)) {
    warning("snaive cannot be used to generate forecasts with: fc horizon > 2 * ts_frequency")
    return(NULL)
  }
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <- sample_split[["train"]]
    x_test <- sample_split[["test"]]
    md <- base::do.call(forecast::snaive, c(base::list(x_train), snaive_arg))
    fc <- forecast::forecast(md, h = fc_horizon)
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Bayesian Structural Time Series Model
#' @description Function to apply the \code{\link[bsts]{bsts}} function from the \code{bsts} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param bsts_arg A list, optional arguments to pass to the \code{\link[bsts]{bsts}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_bsts(AirPassengers,
#'                        fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_bsts(AirPassengers,
#'                        fc_horizon = 12,
#'                        backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_bsts(AirPassengers,
#'                        fc_horizon = 6,
#'                        backtesting_opt = list(use_bt = TRUE,
#'                                               nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_bsts <- function(ts_data_xts,
                             fc_horizon = 12,
                             backtesting_opt = NULL,
                             save_fc_to_file = NULL,
                             bsts_arg = NULL,
                             ...){
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_output <- ss <- base::list()
  md <- fc <- NULL
  if (base::is.null(bsts_arg)) {
    bsts_arg <- base::list(linear_trend = TRUE,
                           seasonal = TRUE,
                           niter = 1000,
                           ping = 0,
                           family = "gaussian",
                           seed = 1234)
  } else {
    if ("linear_trend" %in% base::names(bsts_arg)) {
      if (!bsts_arg$linear_trend %in% c(TRUE, FALSE)) {
        warning("The value of the 'linear_trend' argument of the bsts model is invalid, using default (TRUE)")
        bsts_arg$linear_trend <- TRUE
      }
    } else {
      warning("The 'linear_trend' was not defined, using TRUE as default")
      bsts_arg$linear_trend <- TRUE
    }
    if ("seasonal" %in% base::names(bsts_arg)) {
      if (!bsts_arg$seasonal %in% c(TRUE, FALSE)) {
        warning("The value of the 'seasonal' argument of the bsts model is invalid, using TRUE as default")
        bsts_arg$seasonal <- TRUE
      }
    } else {
      warning("The 'seasonal' argument was not defined, using TRUE as default")
      bsts_arg$seasonal <- TRUE
    }
    if ("niter" %in% base::names(bsts_arg)) {
      if (!base::is.numeric(bsts_arg$niter)) {
        warning("The value of the 'niter' argument of the bsts model is invalid, setting the argument to 1000")
        bsts_arg$niter <- 1000
      } else if (bsts_arg$niter%%1 != 0) {
        warning("The value of the 'niter' argument of the bsts model is not integer, setting the argument to 1000")
        bsts_arg$niter <- 1000
      }
    } else {
      warning("The 'niter' argument was not defined, setting the argument to 1000")
      bsts_arg$niter <- 1000
    }
    if ("ping" %in% base::names(bsts_arg)) {
      if (!base::is.numeric(bsts_arg$ping)) {
        warning("The value of the 'ping' argument of the bsts model is invalid, setting the argument to 100")
        bsts_arg$ping <- 100
      } else if (bsts_arg$ping%%1 != 0) {
        warning("The value of the 'ping' argument of the bsts model is not integer, setting the argument to 100")
        bsts_arg$ping <- 1000
      }
    } else {
      warning("The 'ping' argument was not defined, setting the argument to 100")
      bsts_arg$ping <- 100
    }
    if ("seed" %in% base::names(bsts_arg)) {
      if (!base::is.numeric(bsts_arg$seed)) {
        warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
        bsts_arg$seed <- 1234
      } else if (bsts_arg$seed%%1 != 0) {
        warning("The value of the 'seed' argument of the bsts model is not integer, setting the argument to 1234")
        bsts_arg$seed <- 1234
      }
    } else {
      warning("The 'seed' argument was not defined, setting the argument to 1234")
      bsts_arg$seed <- 1234
    }
    if ("family" %in% base::names(bsts_arg)) {
      if (!bsts_arg$family %in% c("gaussian", "logit",
                                  "poisson", "student")) {
        warning("The value of the 'family' argument of the bsts model is invalid, using 'gaussian' as default")
        bsts_arg$family <- "gaussian"
      }
    } else {
      warning("The value of the 'family' argument is missing, using 'gaussian' as default")
      bsts_arg$family <- "gaussian"
    }
  }
  if (bsts_arg$linear_trend) {
    ss <- bsts::AddLocalLinearTrend(ss, ts_data_xts)
  }
  if (bsts_arg$seasonal) {
    ss <- bsts::AddSeasonal(ss, ts_data_xts, nseasons = stats::frequency(ts_data_xts))
  }
  ts_contiguous_data <- add_placeholders(ts_data_xts, fc_horizon, backtesting_opt)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         backtesting_opt = backtesting_opt)
    x_train <- sample_split[["train"]]
    x_test <- sample_split[["test"]]
    md <- bsts::bsts(x_train,
                     state.specification = ss,
                     niter = bsts_arg$niter,
                     ping = bsts_arg$ping,
                     seed = bsts_arg$seed,
                     family = bsts_arg$family)
    fc <- stats::predict(md, horizon = fc_horizon,
                         quantiles = c(0.025, 0.975))
    base::eval(base::parse(text = base::paste("model_output$period_",
                                              bt_iter, "$fc <- fc",
                                              sep = "")))
  }
  return(model_output)
}

#' Long-Short Term Memory Network
#' @description Function to apply lstm networks (\code{\link[keras]{layer_lstm}}) from the \code{keras} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param xreg_xts A univariate or multivariate ts, mts or xts object, optional external regressors
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param lstm_keras_arg A list, optional arguments to pass to the lstm network
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_lstm_keras(AirPassengers,
#'                              fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_lstm_keras(AirPassengers,
#'                              fc_horizon = 12,
#'                              backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_lstm_keras(AirPassengers,
#'                              fc_horizon = 6,
#'                              backtesting_opt = list(use_bt = TRUE,
#'                                                     nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_lstm_keras <- function(ts_data_xts,
                                   fc_horizon = 12,
                                   xreg_xts = NULL,
                                   backtesting_opt = NULL,
                                   save_fc_to_file = NULL,
                                   lstm_keras_arg = NULL,
                                   ...) {
  `%>%` <- magrittr::`%>%`
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_output <- base::list()
  if (base::is.null(lstm_keras_arg)) {
    lstm_keras_arg = base::list(valid_set_size = stats::frequency(ts_data_xts),
                                stateful = FALSE,
                                nb_stacked_layers = 0,
                                lag_setting = stats::frequency(ts_data_xts),
                                loss = "mean_absolute_error",
                                lr = 0.001,
                                momentum = 0.05,
                                dropout = 0.2,
                                recurrent_dropout = 0.2,
                                nb_units = 100,
                                nb_epochs = 50,
                                nb_timesteps = stats::frequency(ts_data_xts),
                                batch_size = 1,
                                optimizer_type = "adam",
                                patience = 10,
                                verbose = TRUE,
                                seed = NULL,
                                time_features = c("month",
                                                  "year"))
  } else {
    if ("valid_set_size" %in% base::names(lstm_keras_arg)) {
      if (!base::is.numeric(lstm_keras_arg$valid_set_size)) {
        warning("The validation set size argument is invalid, setting to default: frequency of the data")
        lstm_keras_arg$valid_set_size <- stats::frequency(ts_data_xts)
      }
    } else {
      warning("The validation set size argument is missing, setting to default: frequency of the data")
      lstm_keras_arg$valid_set_size <- stats::frequency(ts_data_xts)
    }
    if ("stateful" %in% base::names(lstm_keras_arg)) {
      if (!base::is.logical(lstm_keras_arg$stateful)) {
        warning("The value of the stateful argument is invalid, using FALSE as default.")
        lstm_keras_arg$stateful <- FALSE
      }
    } else {
      warning("The value of the stateful argument is missing, using FALSE as default.")
      lstm_keras_arg$stateful <- FALSE
    }
    if ("nb_stacked_layers" %in% base::names(lstm_keras_arg)) {
      if (!base::is.numeric(lstm_keras_arg$nb_stacked_layers)) {
        warning("The value of the number of stacked layers is invalid, using 0 as default")
        lstm_keras_arg$nb_stacked_layers <- 0
      }
    } else {
      warning("The value of the number of stacked layers is missing, using 0 as default")
      lstm_keras_arg$nb_stacked_layers <- 0
    }
    if (!"lag_setting" %in% base::names(lstm_keras_arg)) {
      warning("The number of lags is missing, using the frequency of the data as default")
      lstm_keras_arg$lag_setting <- stats::frequency(ts_data_xts)
    }
    if (!"loss" %in% base::names(lstm_keras_arg)) {
      warning("The value of the loss function is missing, using 'mean_absolute_error' as default")
      lstm_keras_arg <- "mean_absolute_error"
    }
    if (!"lr" %in% base::names(lstm_keras_arg)) {
      warning("The value of the learning rate is missing, using 0.001 as default")
      lstm_keras_arg$lr <- 0.001
    }
    if (!"momentum" %in% base::names(lstm_keras_arg)) {
      warning("The value of the momentumn is missing, using 0.05 as default")
      lstm_keras_arg$momentum <- 0.05
    }
    if (!"dropout" %in% base::names(lstm_keras_arg)) {
      warning("The value of the dropout rate is missing, using 0.2 as default")
      lstm_keras_arg$dropout <- 0.2
    }
    if (!"recurrent_dropout" %in% base::names(lstm_keras_arg)) {
      warning("The value of the recurrent dropout is missing, using 0.2 as default")
      lstm_keras_arg$recurrent_dropout <- 0.2
    }
    if (!"nb_units" %in% base::names(lstm_keras_arg)) {
      warning("The value of the number of units in LSTM cell is missing, using 100 as default")
      lstm_keras_arg$nb_units <- 100
    }
    if (!"nb_epochs" %in% base::names(lstm_keras_arg)) {
      warning("The value of the number of epochs is missing, using 50 as default")
      lstm_keras_arg$nb_epochs <- 50
    }
    if (!"nb_timesteps" %in% base::names(lstm_keras_arg)) {
      warning("The value of the number of time steps is missing, using the frequency of the data as default")
      lstm_keras_arg$nb_timesteps <- stats::frequency(ts_data_xts)
    }
    if (!"batch_size" %in% base::names(lstm_keras_arg)) {
      warning("The value of the batch size is missing, using 1 as default")
      lstm_keras_arg$batch_size <- 1
    }
    if (!"optimizer_type" %in% base::names(lstm_keras_arg)) {
      warning("The type of optimizer to apply is missing, using 'adam' as default")
      lstm_keras_arg$optimizer_type <- "adam"
    }
    if (!"patience" %in% base::names(lstm_keras_arg)) {
      warning("The value of the loss function is missing, using 10 as default")
      lstm_keras_arg$patience <- 10
    }
    if (!"verbose" %in% base::names(lstm_keras_arg)) {
      warning("The value of verbose is missing, using TRUE as default")
      lstm_keras_arg$verbose <- TRUE
    }
    if (!"time_features" %in% base::names(lstm_keras_arg)) {
      warning("The value of time features to select is missing, using 'month' and 'year' as default")
      lstm_keras_arg$time_features <- c("month", "year")
    }
    if ("seed" %in% base::names(lstm_keras_arg)) {
      if (!base::is.numeric(lstm_keras_arg$seed)) {
        warning("The value of seed is invalid, using NULL as default")
        lstm_keras_arg$seed <- NULL
      }
    } else {
      warning("The value of seed is missing, using NULL as default")
      lstm_keras_arg$seed <- NULL
    }
  }
  base::set.seed(lstm_keras_arg$seed)
  seed <- base::as.integer(stats::runif(1, min = 1, max = 9999))
  keras::use_session_with_seed(seed,
                               disable_gpu = TRUE,
                               disable_parallel_cpu = TRUE)
  callbacks <- base::list(keras::callback_early_stopping(patience = lstm_keras_arg$patience))
  ts_contiguous_data <-
    add_placeholders(ts_data_xts, fc_horizon, backtesting_opt) %>%
    add_features(xreg_xts)
  ts_name <- base::colnames(ts_data_xts)
  ts_freq <- stats::frequency(ts_data_xts)
  for (bt_iter in 1:backtesting_opt$nb_iters) {
    sample_split <- split_train_test_set(ts_contiguous_data,
                                         fc_horizon = fc_horizon,
                                         nb_iter = bt_iter,
                                         valid_set_size = lstm_keras_arg$valid_set_size,
                                         backtesting_opt = backtesting_opt)
    ts_train <-
      sample_split[["train"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate("index" = zoo::index(sample_split[["train"]]) %>% lubridate::as_date()) %>%
      dplyr::mutate("key" = "Training") %>%
      timetk::tk_augment_timeseries_signature()
    ts_valid <-
      sample_split[["valid"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate("index" = zoo::index(sample_split[["valid"]]) %>% lubridate::as_date()) %>%
      dplyr::mutate("key" = "Validation") %>%
      timetk::tk_augment_timeseries_signature()
    ts_test <-
      sample_split[["test"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate("index" = zoo::index(sample_split[["test"]]) %>% lubridate::as_date()) %>%
      dplyr::mutate("key" = "Test") %>%
      timetk::tk_augment_timeseries_signature()
    ts_data <-
      dplyr::bind_rows(ts_train, ts_valid, ts_test) %>%
      dplyr::select(base::list(colnames(ts_contiguous_data),
                               lstm_keras_arg$time_features,
                               "key") %>%
                      base::unlist())
    normalization_step <- normalize_data(ts_data)
    normalized_data <- normalization_step[["data"]]
    mean_history <- normalization_step[["scalers"]][base::colnames(ts_data_xts)][, 'mean_history']
    scale_history <- normalization_step[["scalers"]][base::colnames(ts_data_xts)][, 'scale_history']
    data_with_tsteps <- add_timesteps(data_df = normalized_data,
                                      fc_horizon = fc_horizon,
                                      valid_set_size = lstm_keras_arg$valid_set_size,
                                      tsteps = lstm_keras_arg$nb_timesteps,
                                      lag_setting = lstm_keras_arg$lag_setting,
                                      backtesting_opt = backtesting_opt)
    nb_features <-
      base::colnames(data_with_tsteps)[!base::colnames(data_with_tsteps) %in% c("key", "y")] %>%
      base::length() / lstm_keras_arg$nb_timesteps
    y_train_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Training") %>%
      dplyr::select(y) %>%
      base::as.matrix() %>%
      reshape_Y()
    y_valid_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Validation") %>%
      dplyr::select(y) %>%
      base::as.matrix() %>%
      reshape_Y()
    y_test_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Test") %>%
      dplyr::select(y) %>%
      base::as.matrix() %>%
      reshape_Y()
    x_train_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Training") %>%
      dplyr::select(-y, -key) %>%
      base::as.matrix() %>%
      reshape_X(.,
                tsteps = lstm_keras_arg$nb_timesteps,
                nb_features = nb_features)
    x_valid_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Validation") %>%
      dplyr::select(-y, -key) %>%
      base::as.matrix() %>%
      reshape_X(.,
                tsteps = lstm_keras_arg$nb_timesteps,
                nb_features = nb_features)
    x_test_tensor <-
      data_with_tsteps %>%
      dplyr::filter(key == "Test") %>%
      dplyr::select(-y, -key) %>%
      base::as.matrix() %>%
      reshape_X(.,
                tsteps = lstm_keras_arg$nb_timesteps,
                nb_features = nb_features)
    #### modelling ####
    model <- keras::keras_model_sequential()
    if (lstm_keras_arg$nb_stacked_layers > 0) {
      model %>%
        keras::layer_lstm(
          units = lstm_keras_arg$nb_units,
          batch_input_shape = c(lstm_keras_arg$batch_size,
                                lstm_keras_arg$nb_timesteps,
                                nb_features),
          dropout = lstm_keras_arg$dropout,
          recurrent_dropout = lstm_keras_arg$recurrent_dropout,
          return_sequences = TRUE,
          stateful=lstm_keras_arg$stateful)
      i <- lstm_keras_arg$nb_stacked_layers
      while (i > 1) {
        model %>%
          keras::layer_lstm(
            units = lstm_keras_arg$nb_units,
            dropout = lstm_keras_arg$dropout,
            recurrent_dropout = lstm_keras_arg$recurrent_dropout,
            return_sequences = TRUE,
            stateful = lstm_keras_arg$stateful)
        i <- i - 1
      }
      model %>%
        keras::layer_lstm(
          units = lstm_keras_arg$nb_units,
          dropout = lstm_keras_arg$dropout,
          recurrent_dropout = lstm_keras_arg$recurrent_dropout,
          return_sequences = FALSE,
          stateful = lstm_keras_arg$stateful) %>%
        keras::layer_dense(units = 1)
    } else {
      model %>%
        keras::layer_lstm(
          units = lstm_keras_arg$nb_units,
          batch_input_shape = c(lstm_keras_arg$batch_size,
                                lstm_keras_arg$nb_timesteps,
                                nb_features),
          dropout = lstm_keras_arg$dropout,
          recurrent_dropout = lstm_keras_arg$recurrent_dropout,
          return_sequences = FALSE,
          stateful=lstm_keras_arg$stateful) %>%
        keras::layer_dense(units = 1)
    }
    model %>%
      keras::compile(
        loss = lstm_keras_arg$loss,
        optimizer = lstm_keras_arg$optimizer_type,
        metrics = list(lstm_keras_arg$loss))
    if (length(x_valid_tensor) == 0) {
      lstm_validation_data <- NULL
    } else {
      lstm_validation_data <- list(x_valid_tensor, y_valid_tensor)
    }
    if (!lstm_keras_arg$stateful) {
      model_fit <-
        model %>%
        keras::fit(x = x_train_tensor,
                   y = y_train_tensor,
                   callbacks = callbacks,
                   validation_data = lstm_validation_data,
                   batch_size = lstm_keras_arg$batch_size,
                   epochs = lstm_keras_arg$nb_epochs,
                   shuffle = FALSE,
                   verbose = lstm_keras_arg$verbose)
    }else{
      for (epoch_iter in 1:lstm_keras_arg$nb_epochs) {
        model_fit <-
          model %>% keras::fit(x = x_train_tensor,
                               y = y_train_tensor,
                               validation_data = lstm_validation_data,
                               callbacks = callbacks,
                               batch_size = lstm_keras_arg$batch_size,
                               epochs = 1,
                               shuffle = FALSE,
                               verbose = lstm_keras_arg$verbose)
        model %>% keras::reset_states()
        base::cat("Epoch: ", epoch_iter)
      }
    }

    new_x_test_data <- x_test_tensor
    pred_list <- NULL
    for (i in 1:fc_horizon) {
      # generate a one-step ahead forecast
      prediction <- model %>%
        stats::predict(new_x_test_data[i, ,] %>%
                         array(data = .,
                               dim = c(1,
                                       lstm_keras_arg$nb_timesteps,
                                       nb_features)),
                       batch_size = lstm_keras_arg$batch_size) %>%
        .[, 1]
      pred_list <- c(pred_list, prediction)
      if (i < fc_horizon) {
        first_tstep <- base::max((lstm_keras_arg$nb_timesteps - i + 1), 0)
        new_x_test_data[i + 1,
                        first_tstep:lstm_keras_arg$nb_timesteps,
                        1] <-
          utils::tail(pred_list, lstm_keras_arg$nb_timesteps)
      }
    }
    y_predict_matrix <-
      pred_list %>%
      base::matrix(., nrow = fc_horizon, ncol = 1) * scale_history + mean_history
    tbl1 <-
      dplyr::bind_rows(
        ts_train,
        ts_valid,
        ts_test) %>%
      dplyr::select("index", base::colnames(ts_data_xts), "key") %>%
      dplyr::mutate(key = "actual")
    tbl2 <-
      ts_test %>%
      dplyr::select("index", "key") %>%
      base::cbind(y_predict_matrix) %>%
      {
        colnames(.) <- c("index", "key", base::colnames(ts_data_xts))
        .
      }


    ret <- list(tbl1, tbl2) %>%
      purrr::reduce(time_bind_rows, index = index) %>%
      dplyr::arrange(key, index) %>%
      dplyr::mutate(key = forcats::as_factor(key))

    fc <- stats::ts(tbl2$value,
                    start = c(lubridate::year(first_pred_date),
                              lubridate::month(first_pred_date)),
                    frequency = stats::frequency(ts_data_xts))

  }
  return(model_output)
}

#' Automated Machine Learning
#' @description Function to apply the \code{\link[h2o]{h2o.automl}} function from the \code{h2o} package on
#' time series data.
#' @param ts_data_xts A univariate ts or xts object
#' @param fc_horizon An integer, the forcasting horizon
#' @param xreg_xts A univariate or multivariate ts, mts or xts object, optional external regressors
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt: A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters: An integer, to determine the number of backtesting operations to apply
#'
#'  method: A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size: A string, to determine whether the training set size should expand or remain fixed across backtesting operations
#'
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param automl_h2o_arg A list, optional arguments to pass to the \code{\link[h2o]{h2o.automl}} function
#' @examples
#' library(datasets)
#'
#' # Generate forecasts on future dates
#' fc <- generate_fc_automl_h2o(AirPassengers,
#'                              fc_horizon = 12)
#'
#' # Generate forecasts on past dates to analyze performance
#' fc <- generate_fc_automl_h2o(AirPassengers,
#'                              fc_horizon = 12,
#'                              backtesting_opt = list(use_bt = TRUE))
#'
#' # Generate forecasts on past dates with multiple iterations and a rolling window
#' fc <- generate_fc_automl_h2o(AirPassengers,
#'                              fc_horizon = 6,
#'                              backtesting_opt = list(use_bt = TRUE,
#'                                                     nb_iters = 6))
#' @return A list, forecast object for each forecasted period
#' @export
generate_fc_automl_h2o <- function(ts_data_xts,
                                   fc_horizon = 12,
                                   backtesting_opt = NULL,
                                   save_fc_to_file = NULL,
                                   automl_h2o_arg = list(max_models = 5,
                                                         stopping_metric = "MAE"),
                                   ...){
  # options:
  # nb_threads
  # use_bt
  # nb_periods <- backtesting_opt
  # fc_horizon
  # train_set_size
  # valid_set_size
  # test_set_size
  `%>%` <- magrittr::`%>%`
  h2o::h2o.init(port = 54321, nthreads = nb_threads)
  if (!backtesting_opt$use_bt & nb_periods != 1) {
    print("When forecasting (backtesting=FALSE),
          the number of periods is automatically set to 1!
          You do not need to explicitly set this argument.")
  }
  ID <- base::colnames(ts_data_xts)
  ts_contiguous_data <- timeSeries::na.contiguous(ts_data_xts)
  valid_set_size <- 12
  test_set_size <- 12
  nb_periods <- backtesting_opt$nb_iters
  train_set_size <-
    base::length(ts_contiguous_data) -
    valid_set_size -
    test_set_size -
    backtesting_opt$use_bt * fc_horizon -
    nb_periods + 1

  if (base::length(ts_contiguous_data) -
      backtesting_opt$use_bt * fc_horizon -
      nb_periods + 1 <= 0) {
    stop("The length of the ts obj is not long enough!")
  }
  ts_data_df <-
    ts_contiguous_data %>%
    base::as.matrix() %>%
    base::rbind(base::matrix(base::rep(NaN, fc_horizon * backtesting_opt$use_bt))) %>%
    {
      base::rownames(.) <-
        c(zoo::index(ts_contiguous_data),
                     timetk::tk_make_future_timeseries(zoo::index(ts_contiguous_data),
                                                       fc_horizon * backtesting_opt$use_bt)) %>%
        base::as.character()
      .
    } %>%
    xts::xts(., order.by = base::rownames(.) %>% zoo::as.yearmon() %>% lubridate::as_date()) %>%
    base::as.data.frame() %>%
    tibble::rownames_to_column("Date")
  ts_augmented_data <- timetk::tk_augment_timeseries_signature(ts_data_df)
  ts_cleaned <-
    ts_augmented_data %>%
    dplyr::select(-diff)

  for (bt_iter in 1:nb_periods) {
    last_train_date <- ts_augmented_data$Date[train_set_size
                                              + period_iter
                                              - 1]
    last_valid_date <- ts_augmented_data$Date[train_set_size
                                              + valid_set_size
                                              + period_iter
                                              - 1]
    last_test_date <- ts_augmented_data$Date[train_set_size
                                             + valid_set_size
                                             + test_set_size
                                             + period_iter
                                             - 1]
    last_pred_date <- ts_augmented_data$Date[train_set_size
                                             + valid_set_size
                                             + test_set_size
                                             + fc_horizon
                                             + period_iter
                                             - 1]
    train_h2o <-
      ts_cleaned %>%
      dplyr::filter(Date <= last_train_date) %>%
      dplyr::select(-Date) %>%
      dplyr::mutate_if(base::is.ordered, ~base::as.character(.)
                       %>% base::as.factor()) %>%
      h2o::as.h2o()
    valid_h2o <-
      ts_cleaned %>%
      dplyr::filter(Date <= last_valid_date & Date > last_train_date) %>%
      dplyr::select(-Date) %>%
      dplyr::mutate_if(base::is.ordered, ~base::as.character(.)
                       %>% base::as.factor()) %>%
      h2o::as.h2o()
    test_h2o <-
      ts_cleaned %>%
      dplyr::filter(Date <= last_test_date & Date > last_valid_date) %>%
      dplyr::select(-Date) %>%
      dplyr::mutate_if(base::is.ordered, ~base::as.character(.)
                       %>% base::as.factor()) %>%
      h2o::as.h2o()
    input_pred_h2o <-
      ts_cleaned %>%
      dplyr::filter(Date <= last_pred_date & Date > last_test_date) %>%
      dplyr::select(-Date) %>%
      dplyr::mutate_if(base::is.ordered, ~base::as.character(.)
                       %>% base::as.factor()) %>%
      h2o::as.h2o()
    y <- colnames(ts_data_xts)
    x <- dplyr::setdiff(base::names(train_h2o), y)
    automl_models_h2o <- h2o::h2o.automl(x = x,
                                         y = y,
                                         training_frame = train_h2o,
                                         validation_frame = valid_h2o,
                                         leaderboard_frame = test_h2o,
                                         max_models = max_nb_models,
                                         max_runtime_secs = max_runtime_secs,
                                         stopping_metric = "MAE",
                                         seed = 1234,
                                         exclude_algos = algos_to_exclude)
    h2o_model <- automl_models_h2o@leader
    pred_h2o <- h2o::h2o.predict(h2o_model, input_pred_h2o)
    h2o::h2o.removeAll()
  }
  return(model_output)
}
