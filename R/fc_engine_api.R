#' Forecasting Engine API
#'
#' This function enables the user to select different forecasting algorithms ranging from
#' traditional time series models (i.e. ARIMA, ETS, STL) to machine learning methods (i.e. LSTM, AutoML).
#'
#' @param mts_data ts, mts or xts object
#' @return A list of forecasts for each time series
#' @export
generate_fc <- function(mts_data, fc_horizon = 1,
                        xreg_data = NULL,
                        backtesting_opt = list(use_backtesting = FALSE,
                                               backtesting_nb_iters = 1,
                                               backtesting_method = c("rolling",
                                                                      "moving"),
                                               backtesting_set_size = c("expanding",
                                                                        "fixed")),
                        model_names = c("arima", "ets", "tbats", "bsts",
                                        "stl", "snaive", "nnetar"),
                        models_args = NULL,
                        save_fc_to_file = NULL,
                        use_parallel = FALSE,
                        ...) {
  `%>%` <- magrittr::`%>%`
  model_output <- base::list()
  mts_data_xts <- check_data_sv_as_xts(mts_data, default_colname = "time_series")
  xreg_data_xts <- check_data_sv_as_xts(xreg_data, default_colname = "feature")
  if (!base::is.null(xreg_data_xts)) {
    keys_in_col <- base::colnames(xreg_data_xts) %>% stringr::str_detect("__")
    print(base::paste("Info about specified regressors: \n",
                      "Number of total features: ",
                      base::ncol(xreg_data_xts), "\n",
                      "Number of shared features (colnames w/o '__'): ",
                      base::sum(!keys_in_col), "\n",
                      "Number of ts specific features (ts_name + '__' + feature_name): ",
                      base::sum(keys_in_col),
                      sep = ""))
  }
  fc_horizon <- check_fc_horizon(fc_horizon)
  model_names <- check_model_names(model_names)
  models_args <- check_models_args(models_args, model_names)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  use_parallel <- check_use_parallel(use_parallel)

  ind_seq <- base::seq(base::ncol(mts_data_xts))
  if (use_parallel) {
    nb_cores <- parallel::detectCores()
    cl <- parallel::makeCluster(nb_cores)
    doParallel::registerDoParallel(cl)
    foreach::foreach(ind = ind_seq) foreach::%dopar% {
      source("./R/fc_models.R")
      source("./R/checks.R")
      source("./R/preprocessing.R")
      model_names_parall_proc <- model_names[model_names != "automl_h2o"]
      ts_data_xts <- mts_data_xts[, ind]
      ts_colname <- base::colnames(ts_data_xts)
      for (model_name in model_names_parall_proc) {
        base::eval(base::parse(text = base::paste("model_output$", ts_colname, "$", model_name, " <- ",
                                                  "generate_fc_", model_name, "(",
                                                  "ts_data_xts = ts_data_xts, ",
                                                  "xreg_xts = xreg_data_xts, ",
                                                  "fc_horizon = fc_horizon, ",
                                                  "backtesting_opt = backtesting_opt, ",
                                                  "save_fc_to_file = save_fc_to_file, ",
                                                  model_name, "_arg = models_args$", model_name, "_arg)",
                                                  sep = "")))
      }
    }
    parallel::stopCluster(cl)
    foreach::foreach(ind = ind_seq) foreach::%do% {
      model_names_parall_proc <- model_names[model_names == "automl_h2o"]
      ts_data_xts <- mts_data_xts[, ind]
      ts_colname <- base::colnames(ts_data_xts)
      for (model_name in model_names_parall_proc) {
        base::eval(base::parse(text = base::paste("model_output$", ts_colname, "$", model_name, " <- ",
                                                  "generate_fc_", model_name, "(",
                                                  "ts_data_xts = ts_data_xts, ",
                                                  "xreg_xts = xreg_data_xts, ",
                                                  "fc_horizon = fc_horizon, ",
                                                  "backtesting_opt = backtesting_opt, ",
                                                  "save_fc_to_file = save_fc_to_file, ",
                                                  model_name, "_arg = models_args$", model_name, "_arg)",
                                                  sep = "")))
      }
    }
  } else {
    foreach::foreach(ind = ind_seq) foreach::%do% {
      model_names_parall_proc <- model_names
      ts_data_xts <- mts_data_xts[,ind]
      ts_colname <- base::colnames(ts_data_xts)
      for (model_name in model_names_parall_proc) {
        base::eval(base::parse(text = base::paste("model_output$", ts_colname, "$", model_name, " <- ",
                                                  "generate_fc_", model_name, "(",
                                                  "ts_data_xts = ts_data_xts, ",
                                                  "xreg_xts = xreg_data_xts, ",
                                                  "fc_horizon = fc_horizon, ",
                                                  "backtesting_opt = backtesting_opt, ",
                                                  "save_fc_to_file = save_fc_to_file, ",
                                                  model_name, "_arg = models_args$", model_name, "_arg)",
                                                  sep = "")))
      }
    }
  }
  return(model_output)
}
