#' Check backtesting options
#' @description
#' This function ensures that the selected backtesting options are valid.
#' If not, the function throws an error. If the options are NULL, then default
#' values will be applied.
#' @param backtesting_opt A list, options for the backtesting program:
#'
#'  use_bt - A boolean, to determine whether to apply backtesting or to generate forcasts on future dates
#'
#'  nb_iters - An integer, to determine the number of backtesting operations to apply
#'
#'  method - A string, to determine whether to use a rolling or a moving forecasting window
#'
#'  sample_size - A string, to determine whether the training set size should expand or remain
#'  fixed across backtesting operations
#'
#' @return A list of backtesting options
#' @export
check_backtesting_opt <- function(backtesting_opt) {
  `%>%` <- magrittr::`%>%`
  if (base::is.null(backtesting_opt) & !base::is.list(backtesting_opt)) {
    warning("No backtesting plan has been specified, setting use_bt to FALSE by default")
    backtesting_opt <-
      base::list(use_bt = FALSE,
                 nb_iters = 1,
                 method = c("rolling",
                             "moving"),
                 sample_size = c("expanding",
                                 "fixed"))
  }
  if (!base::is.list(backtesting_opt)) {
    warning("No backtesting plan has been specified, setting use_bt to FALSE by default")
    backtesting_opt <-
      base::list(use_bt = FALSE,
                 nb_iters = 1,
                 method = c("rolling",
                            "moving"),
                 sample_size = c("expanding",
                                 "fixed"))
  }
  if ("use_bt" %in% base::names(backtesting_opt)) {
    if (!base::is.logical(backtesting_opt$use_bt)) {

      warning("The value of the backtesting plan is invalid, using FALSE as default")
      backtesting_opt$use_bt <- FALSE
    }
  } else {
    warning("The value of the backtesting plan is missing, using FALSE as default")
    backtesting_opt$use_bt <- FALSE
  }
  if ("nb_iters" %in% base::names(backtesting_opt)) {
    if (!is.numeric(backtesting_opt$nb_iters)) {
      warning(base::paste("The value of the number of backtesting operations to perform is invalid, ",
                          "setting to 1 as default",
                          sep = ""))
      backtesting_opt$nb_iters <- 1
    }
  } else {
    warning(base::paste("The value of the number of backtesting operations to perform is missing, ",
                        "setting to 1 as default",
                        sep = ""))
    backtesting_opt$nb_iters <- 1
  }
  if ("method" %in% base::names(backtesting_opt)) {
    if (!backtesting_opt$method[1] %in% c("rolling", "moving")) {
      warning("The value of the backtesting method is invalid, using 'rolling' as default")
      backtesting_opt$method <- c("rolling", "moving")
    }
  } else {
    warning("The value of the backtesting method is missing, using 'rolling' as default")
    backtesting_opt$method <- c("rolling", "moving")
  }
  if ("sample_size" %in% base::names(backtesting_opt)) {
    if (!backtesting_opt$sample_size[1] %in% c("expanding", "fixed")) {
      warning("The value of the sample size is invalid, using 'expanding' as default")
      backtesting_opt$sample_size <- c("expanding", "fixed")
    }
  } else {
    warning("The value of the sample size is missing, using 'expanding' as default")
    backtesting_opt$sample_size <- c("expanding", "fixed")
  }
  if (!backtesting_opt$use_bt) {
    if (backtesting_opt$nb_iters != 1) {
      warning(base::paste("As no backtesting is applied, there will be only one ",
                          "forecasting exercise for each time series!",
                          sep = ""))
      backtesting_opt$nb_iters <- 1
    }
  }
  return(backtesting_opt)
}

#' Check forecasting horizon
#' @description
#' This function ensures that the selected forecasting horizon is valid.
#' If not, the function throws an error.
#' @param fc_horizon A positive integer
#' @return fc_horizon: A positive integer
#' @export
check_fc_horizon <- function(fc_horizon) {
  if (!base::is.numeric(fc_horizon)) {
    stop("The forecasting horizon must be an positive non-zero integer!")
  } else if (fc_horizon != base::as.integer(fc_horizon) | fc_horizon < 1) {
    stop("The forecasting horizon must be an positive non-zero integer!")
  }
  return(fc_horizon)
}

#' Check validation set size
#' @description
#' This function ensures that the selected validation set size is valid.
#' If not, the function throws an error.
#' @param valid_set_size An integer
#' @return valid_set_size: An integer
#' @export
check_valid_set_size <- function(valid_set_size) {
  if (!base::is.numeric(valid_set_size)) {
    stop("The validation set size must be an positive non-zero integer!")
  } else if (valid_set_size != base::as.integer(valid_set_size) | valid_set_size < 0) {
    stop("The validation set size must be an positive non-zero integer!")
  }
  return(valid_set_size)
}

#' Check optional test set size
#' @description
#' This function ensures that the optional test set size is valid. The optional
#' test set size is used to identify the best performing model inside the
#' h2o.automl procedure. If the argument is invalid, the function throws an error.
#' @param tmp_test_set_size A positive integer
#' @return tmp_test_set_size: a positive integer
#' @export
check_tmp_test_set_size <- function(tmp_test_set_size) {
  if (!base::is.numeric(tmp_test_set_size)) {
    stop("The second test set size must be an positive non-zero integer!")
  } else if (tmp_test_set_size != base::as.integer(tmp_test_set_size) | tmp_test_set_size < 0) {
    stop("The second test set size must be an positive non-zero integer!")
  }
  return(tmp_test_set_size)
}

#' Check backtesting iteration number
#' @description
#' This function ensures that the backtesting iteration number is valid.
#' If not, the function throws an error.
#' @param bt_iter A positive integer which is smaller or equal to the
#' number of backtesting operations to perform.
#' @return bt_iter: a positive integer
#' @export
check_backtesting_iter <- function(bt_iter, backtesting_opt = NULL) {
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  if (!base::is.numeric(bt_iter)) {
    stop("The sample number must be an positive non-zero integer!")
  } else if (bt_iter != base::as.integer(bt_iter) | bt_iter < 1) {
    stop("The sample number must be a positive non-zero integer!")
  } else if (bt_iter > backtesting_opt$nb_iters) {
    stop("More iterations performed than specified in the backtesting plan!")
  }
  return(bt_iter)
}

#' Drop punctuations in colnames
#' @description
#' Punctuations in colnames will dropped in order to
#' avoid errors when converting between different data types.
#' @param colnames_vec a vector of strings, colnames to be cleaned
#' @param default_colname a string,
#' @return colnames_vec
#' @export
check_colnames <- function(data_colnames,
                           default_colname = "time_series",
                           nb_colnames = 1) {
  `%>%` <- magrittr::`%>%`
  if (base::is.null(data_colnames)) {
    warning(base::paste("Missing or invalid colnames. Setting to default: '",
                        default_colname,
                        "_'+'nb'",
                        sep = ""))
    data_colnames <-
      base::paste(default_colname,
                  base::seq(1:nb_colnames),
                  sep = "_")
  }
  if (!base::is.character(data_colnames)) {
    stop("Invalid value for colnames argument! Colnames must be passed as a vector of strings")
  }
  if (data_colnames %>%
      stringr::str_detect(pattern = "[^[:alnum:]_]") %>%
      base::sum() > 0) {
    data_colnames <-
      data_colnames %>%
      base::gsub(pattern = "[^[:alnum:]_]",
                 replacement = "")
    warning("Punctuation and whitespaces in colnames are dropped.")
  }
  return(data_colnames)
}

#' Check the time series data and convert to xts
#' @description
#' This function ensures that the input data is of type xts, ts or mts and
#' converts it to an xts object. Furthermore, if no colnames are specified, default
#' colnames will be applied. Moreover, punctuations in colnames will dropped to
#' avoid errors when converting between different data types.
#' @param input_data ts, mts or xts object
#' @return xts object with punctuations dropped in colnames
#' @export
check_data_sv_as_xts <- function(input_data, default_colname = "time_series") {
  `%>%` <- magrittr::`%>%`
  if (base::is.null(input_data)) {
    return(input_data)
  } else if (!base::class(input_data)[1] %in% c("ts", "mts", "xts")) {
    stop("The input data must be of type 'ts', 'mts' or 'xts'")
  } else if (base::class(input_data)[1] %in% c("ts", "mts")) {
    input_data_xts <- xts::as.xts(input_data)
  } else {
    input_data_xts <- input_data
  }
  if (!base::is.character(default_colname)) {
    stop("The default colname must be a string!")
  }
  base::colnames(input_data_xts) <-
    check_colnames(base::colnames(input_data_xts),
                   default_colname = default_colname,
                   nb_colnames = base::ncol(input_data_xts))
  return(input_data_xts)
}

#' Check the filepath where forecasts can be saved
#' @description
#' This function ensures that the filepath where forecast can be saved is valid
#' If not the argument is invalid, the function throws an error.
#' @param save_fc_to_file NULL or a valid filepath
#' @return NULL or a valid filepath
#' @export
check_save_fc_to_file <- function(save_fc_to_file) {
  `%>%` <- magrittr::`%>%`
  if (base::is.null(save_fc_to_file)) {
    return(NULL)
  } else if (!base::is.character(save_fc_to_file)) {
    stop("The provided directory must be a string!")
  } else if (!base::dir.exists(file.path(save_fc_to_file))) {
    stop("The provided directory does not exist!")
  } else {
    dir_name <- base::dirname(file.path(save_fc_to_file))
    folder_name <-
      stringr::str_split(save_fc_to_file, pattern = "/") %>%
      base::unlist() %>%
      utils::tail(1)
    # To test permissions: create a new file and delete it:
    file_name <-
      base::gsub(base::as.character(base::Sys.time()),
                 pattern = "[^[:alnum:]_]",
                 replacement = "")
    file_path <- base::paste(dir_name, "/",
                             folder_name, "/",
                             file_name,
                             sep = "")
    unique_file_path <- file_path
    while (base::file.exists(file_path)) {
      unique_file_path <- base::paste(unique_file_path, "1", sep = "")
    }
    utils::write.table("test", unique_file_path)
    base::file.remove(unique_file_path)
    return(save_fc_to_file)
  }
}

#' Check model names
#' @description
#' This function ensures that the selected model names are valid.
#' If not, the function throws an error.
#' @param model_names A list or vector of strings
#' @return model_names: vector of strings
#' @export
check_model_names <- function(model_names) {
  `%>%` <- magrittr::`%>%`
  available_models <- c("arima", "ets", "tbats", "bsts",
                        "stl", "snaive", "nnetar", "automl_h2o",
                        "lstm_keras")
  if (base::is.null(model_names)) {
    model_names <- available_models
  } else if (!base::is.list(model_names) & !base::is.character(model_names)) {
    stop("List of models to select must be a list or a character!")
  }
  model_names_unlist <-
    model_names %>%
    base::unlist() %>%
    base::unique()
  if (base::sum(model_names_unlist %in% available_models) != base::length(model_names_unlist)) {
    stop(base::paste("Invalid model selected! List of models allowed:",
                     base::paste(available_models, collapse = ", ")))
  }
  return(model_names_unlist)
}

#' Check models' arguments
#' @description
#' This function ensures that the models' arguments are a list. Furthermore, the
#' function checks if these arguments match the selected model names, otherwise they
#' will be dropped.
#' @param models_args A list
#' @return models_args: A list
#' @export
check_models_args <- function(models_args, model_names = NULL) {
  if (base::is.null(models_args)){
    return(base::list())
  } else if (!is.list(models_args)) {
    stop("The models arguments must be passed as a list!")
  } else if (base::is.null(base::names(models_args))) {
    return(base::list())
  } else {
    model_names <- check_model_names(model_names)
    valid_models_args <- (base::names(models_args) %in% base::paste(model_names, "_arg", sep = ""))
    if (sum(valid_models_args) < length(names(models_args))) {
      warning(base::paste("The following model arguments do not match the selected models ",
                          "and will be dropped: ",
                          base::paste(base::names(models_args)[!valid_models_args],
                                      collapse = ", "),
                          sep = ""))
    }
    return(models_args[valid_models_args])
  }
}

#' Check the number of selected CPU cores
#' @description
#' This function ensures that the user selects a valid number of CPU cores.
#' If the number of cores is invalid, then use the total number of available cores as default.
#' @param nb_cores A numeric, the number of selected CPU cores
#' @return A numeric, the number of selected CPU cores
#' @export
check_nb_cores <- function(nb_cores) {
  nb_cores_available <- parallel::detectCores()
  if (base::is.null(nb_cores)) {
    warning(base::paste("The selected number of CPU cores is invalid, setting number to all ",
                        "available cores (logical included): ",
                        nb_cores_available,
                        sep = ""))
    nb_cores <- nb_cores_available
  } else if (!base::is.numeric(nb_cores)) {
    warning(base::paste("The selected number of CPU cores is invalid, setting number to all ",
                        "available cores (logical included): ",
                        nb_cores_available,
                        sep = ""))
    nb_cores <- nb_cores_available
  } else if (nb_cores != base::as.integer(nb_cores) | nb_cores < 1) {
    warning(base::paste("The selected number of CPU cores is invalid, setting number to all ",
                        "available cores (logical included): ",
                        nb_cores_available,
                        sep = ""))
    nb_cores <- nb_cores_available
  }
  if (nb_cores > nb_cores_available) {
    warning(base::paste("The selected number of CPU cores is invalid, setting number to all ",
                        "available cores (logical included): ",
                        nb_cores_available,
                        sep = ""))
    nb_cores <- nb_cores_available
  }
  return(nb_cores)
}

#' Check the custom preprocessing function
#' @description
#' This function checks whether the specified function is indeed a function
#' @param fct A function, the custom preprocessing function to handle missing values in the data
#' @return NULL (if no fct is specified) or fct (if fct is specified)
#' @export
check_preprocess_fct <- function(fct) {
  if (base::is.null(fct)) {
    warning(base::paste("No custom preprocessing function has been specified! ",
                        "Setting to default: default_prepro_fct",
                        sep = ""))
    return(default_prepro_fct)
  } else if (base::is.character(fct)) {
    warning(base::paste("Invalid argument for the custom preprocessing function! Argument must be a function. ",
                        "Setting to default: default_prepro_fct",
                        sep = ""))
    return(default_prepro_fct)
  } else if (base::is.function(fct)) {
    return(fct)
  } else if (base::is.list(fct)) {
    if (base::length(fct) == 0) {
      warning(base::paste("Invalid argument for the custom preprocessing function! Argument must be a function. ",
                          "Setting to default: default_prepro_fct",
                          sep = ""))
      return(default_prepro_fct)
    } else {
      custom_fct <- fct[[1]]
      if (!base::is.function(custom_fct)) {
        warning(base::paste("No custom preprocessing function has been found in list. Argument must be a function. ",
                            "Setting to default: default_prepro_fct",
                            sep = ""))
        return(default_prepro_fct)
      } else {
        return(fct)
      }
    }
  } else {
    return(default_prepro_fct)
  }
}

#' Check the time identifier
#' @description
#' This function ensures that the user specifies a valid time identifier.
#' @param time_id A POSIXct, created with \code{\link[base]{Sys.time}} and appended to results
#' @return A POSIXct, time identifier
#' @export
check_time_id <- function(time_id) {
  if (!base::is.null(time_id)) {
    if (base::class(time_id)[1] != "POSIXct") {
      warning("The value of the 'time_id' argument is invalid. Using NULL as default.")
      return(NULL)
    } else if (base::length(time_id) > 1) {
      warning("Multiple values of the 'time_id' argument detected. Selecting only the first one.")
      return(time_id[1])
    } else if (base::length(time_id) < 1) {
      warning("No values of the 'time_id' argument detected. Using NULL as default.")
      return(NULL)
    } else {
      return(time_id)
    }
  } else {
    return(NULL)
  }
}

#' Check the period identifier
#' @description
#' This function ensures that the user specifies a valid period identifier.
#' @param period_iter A string, period identifier of format: 'period' + '_' + iter
#' @return A POSIXct, time identifier
#' @export
check_period_iter <- function(period_iter) {
  if (!base::is.character(period_iter)) {
    stop("The value of the 'period_iter' argument is invalid! The argument must be a character.")
  } else if (base::length(period_iter) != 1) {
    stop("The length of the period_iter argument is invalid! The argument must have one value.")
  } else if (!stringr::str_detect(period_iter, pattern = "period_")) {
    stop("The value of the 'period_iter' argument is invalid! Please check its format ('period' + '_' + iter).")
  } else {
    iter <- base::gsub("period_", "", period_iter)
    if (base::is.na(base::as.numeric(iter))) {
      stop("The value of the 'period_iter' argument is invalid! Please check its format ('period' + '_' + iter).")
    } else {
      return(base::paste("period_", iter, sep = ""))
    }
  }
}

valid_md_arima <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (base::nrow(ts_data_xts) < stats::frequency(ts_data_xts) + 1) {
    warning(base::paste("Not enough observations! To use 'arima', there must be more than ",
                        stats::frequency(ts_data_xts)," observations available.",
                        sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_ets <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (base::nrow(ts_data_xts) < 2) {
    warning("Not enough observations! To use 'ets', there must be more than 2 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_snaive <- function(ts_data_xts, fc_horizon) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  fc_horizon <- check_fc_horizon(fc_horizon)
  if (fc_horizon > 2 * stats::frequency(ts_data_xts)) {
    warning("snaive cannot be used to generate forecasts with: fc horizon > 2 * ts_frequency")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_stl <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (stats::frequency(ts_data_xts) <= 1) {
    warning("The data is not a seasonal object! To use 'stl', the data frequency must be higher than 1.
            Otherwise, the seasonal component cannot be estimated.")
    return(FALSE)
  } else if (base::nrow(ts_data_xts) < 2*stats::frequency(ts_data_xts) + 1) {
    warning(base::paste("Not enough observations! To use 'stl', there must be more than ",
                        stats::frequency(ts_data_xts)," observations available.",
                        sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_tbats <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (base::nrow(ts_data_xts) < 2) {
    warning("Not enough observations! To use 'tbats', there must be more than 2 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_nnetar <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (base::nrow(ts_data_xts) < 3) {
    warning("Not enough observations! To use 'nnetar', there must be more than 3 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_bsts <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (base::nrow(ts_data_xts) < 3) {
    warning("Not enough observations! To use 'bsts', there must be more than 3 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_lstm_keras <- function(ts_data_xts, lstm_keras_arg) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  min_nb_obs <-
    (base::max(lstm_keras_arg$lag_setting, lstm_keras_arg$nb_timesteps)
     + lstm_keras_arg$valid_set_size
     + 1)
  if (base::nrow(ts_data_xts) < min_nb_obs) {
    warning(base::paste("Not enough observations! To use 'lstm_keras', there must be more than ",
                        min_nb_obs, " observations available.",
                        sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_md_autml_h2o <- function(ts_data_xts, automl_h2o_arg) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  min_nb_obs <-
    (automl_h2o_arg$valid_set_size
     + automl_h2o_arg$test_set_size)
  if (base::nrow(ts_data_xts) < min_nb_obs) {
    warning(base::paste("Not enough observations! To use 'automl_h2o', ",
                        "there must be more than ",
                        min_nb_obs, " observations available.",
                        sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}
