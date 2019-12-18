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
      warning("The value of the number of backtesting operations to perform is invalid, setting to 1 as default")
      backtesting_opt$nb_iters <- 1
    }
  } else {
    warning("The value of the number of backtesting operations to perform is missing, setting to 1 as default")
    backtesting_opt$nb_iters <- 1
  }
  if ("method" %in% base::names(backtesting_opt)) {
    if (!backtesting_opt$method[1] %in% c("rolling", "moving")) {
      warning("The value of the backtesting method is invalid, using 'rolling' as default")
      backtesting_opt$method <- "rolling"
    }
  } else {
    warning("The value of the backtesting method is missing, using 'rolling' as default")
    backtesting_opt$method <- "rolling"
  }
  if ("sample_size" %in% base::names(backtesting_opt)) {
    if (!backtesting_opt$sample_size[1] %in% c("expanding", "fixed")) {
      warning("The value of the sample size is invalid, using 'expanding' as default")
      backtesting_opt$sample_size <- "expanding"
    }
  } else {
    warning("The value of the sample size is missing, using 'expanding' as default")
    backtesting_opt$sample_size <- "expanding"
  }
  if (!backtesting_opt$use_bt) {
    if (backtesting_opt$nb_iters != 1) {
      warning("As no backtesting is applied, there will be only one forecasting exercise for each time series!")
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
check_valid_set_size <- function(valid_set_size) {
  if (!base::is.numeric(valid_set_size)) {
    stop("The validation set size must be an positive non-zero integer!")
  } else if (valid_set_size != as.integer(valid_set_size) | valid_set_size < 0) {
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
check_tmp_test_set_size <- function(tmp_test_set_size) {
  if (!base::is.numeric(tmp_test_set_size)) {
    stop("The second test set size must be an positive non-zero integer!")
  } else if (tmp_test_set_size != as.integer(tmp_test_set_size) | tmp_test_set_size < 0) {
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

#' Check the time series data and convert to xts
#' @description
#' This function ensures that the input data is of type xts, ts or mts and
#' converts it to an xts object. Furthermore, if no colnames are specified, default
#' colnames will be applied. Moreover, punctuations in colnames will dropped to
#' avoid errors when converting between different data types.
#' @param input_data ts, mts or xts object
#' @return xts object with punctuations dropped in colnames
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
  if (base::is.null(base::colnames(input_data_xts))) {
    base::colnames(input_data_xts) <-
      base::paste(default_colname,
                  base::seq(1:base::ncol(input_data_xts)),
                  sep = "_")
  }
  if (base::colnames(input_data_xts) %>%
      stringr::str_detect(pattern = "[^[:alnum:]_]") %>%
      base::sum() > 0) {
    base::colnames(input_data_xts) <-
      base::colnames(input_data_xts) %>%
      base::gsub(pattern = "[^[:alnum:]_]",
                 replacement = "")
    warning("Punctuation and whitespaces in colnames are dropped.")
  }
  return(input_data_xts)
}

#' Check the filepath where forecasts can be saved
#' @description
#' This function ensures that the filepath where forecast can be saved is valid
#' If not the argument is invalid, the function throws an error.
#' @param save_fc_to_file NULL or a valid filepath
#' @return NULL or a valid filepath
check_save_fc_to_file <- function(save_fc_to_file) {
  if (!base::is.null(save_fc_to_file)) {
    base::dir.create(file.path(save_fc_to_file),
                     showWarnings = FALSE)
    return(save_fc_to_file)
  }
  return(save_fc_to_file)
}

#' Check model names
#' @description
#' This function ensures that the selected model names are valid.
#' If not, the function throws an error.
#' @param model_names A list or vector of strings
#' @return model_names: vector of strings
check_model_names <- function(model_names) {
  `%>%` <- magrittr::`%>%`
  available_models <- c("arima", "ets", "tbats", "bsts",
                        "stl", "snaive", "nnetar", "automl_h2o",
                        "lstm_keras", "hybrid")
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
check_models_args <- function(models_args, model_names = NULL) {
  if (!base::is.null(models_args)){
    if (!base::is.list(models_args)) {
      stop("The models arguments must be passed as a list!")
    } else {
      model_names <- check_model_names(model_names)
      valid_models_args <- (names(models_args) %in% base::paste(model_names, "_arg", sep = ""))
      warning(base::paste("The following model arguments do not match the selected models and will be dropped: ",
                          base::paste(names(models_args)[!valid_models_args],
                                      collapse = ", "),
                          sep = ""))
    }
  }
  return(models_args)
}

#' Check the number of selected CPU cores
#' @description
#' This function ensures that the user selects a valid number of CPU cores.
#' If the number of cores is invalid, then use the total number of available cores as default.
#' @param nb_cores A numeric, the number of selected CPU cores
#' @return A numeric, the number of selected CPU cores
check_nb_cores <- function(nb_cores) {
  nb_cores_available <- parallel::detectCores()
  if (is.null(nb_cores)) {
    warning(paste("The selected number of CPU cores is invalid, setting number to all ",
                  "available cores (logical included): ",
                  nb_cores_available,
                  sep = ""))
    nb_cores <- nb_cores_available
  } else if (!is.numeric(nb_cores)) {
    warning(paste("The selected number of CPU cores is invalid, setting number to all ",
                  "available cores (logical included): ",
                  nb_cores_available,
                  sep = ""))
    nb_cores <- nb_cores_available
  } else if (nb_cores != as.integer(nb_cores) | nb_cores < 1) {
    warning(paste("The selected number of CPU cores is invalid, setting number to all ",
                  "available cores (logical included): ",
                  nb_cores_available,
                  sep = ""))
    nb_cores <- nb_cores_available
  }
  if (nb_cores > nb_cores_available) {
    warning(paste("The selected number of CPU cores is invalid, setting number to all ",
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
check_preprocess_fct <- function(fct) {
  if (!is.null(fct) & !is.function(fct)) {
    if (is.list(fct)) {
      custom_fct <- fct[[1]]
      if (!is.function(custom_fct)) {
        warning("No custom preprocessing function has been found in list. Setting to default: NULL")
        fct <- NULL
      }
    } else {
      fct <- NULL
    }
  }
  return(fct)
}
