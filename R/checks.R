#' Check backtesting options
#' This function ensures that the selected backtesting options are valid.
#' If not, the function throws an error. If the options are NULL, then default
#' values will be applied.
#'
#' @param backtesting_opt NULL or a list
#' @return A list of backtesting options
check_backtesting_opt <- function(backtesting_opt) {
  `%>%` <- magrittr::`%>%`
  if (base::is.null(backtesting_opt)) {
    backtesting_opt <-
      base::list(use_backtesting = FALSE,
                 backtesting_nb_iters = 1,
                 backtesting_method = c("rolling",
                                        "moving"),
                 backtesting_set_size = c("expanding",
                                          "fixed"))
  } else if (!base::is.list(backtesting_opt)) {
    stop("backtesting_opt must be a list!")
  } else if (!base::is.logical(backtesting_opt$use_backtesting)) {
    stop("To use backtesting, please specify TRUE or FALSE as an argument!")
  } else if (!base::is.numeric(backtesting_opt$backtesting_nb_iters)) {
    stop("The number of iterations must be an integer!")
  } else if (backtesting_opt$backtesting_nb_iters != base::as.integer(backtesting_opt$backtesting_nb_iters)) {
    stop("The number of iterations must be an integer!")
  } else if (backtesting_opt$backtesting_nb_iters < 1) {
    stop("The number of iterations must be a positive non-zero integer!")
  } else if (!backtesting_opt$backtesting_method[1] %in% c("rolling", "moving")) {
    stop("The backtesting method must either 'rolling' or 'moving'!")
  } else if (!backtesting_opt$backtesting_set_size[1] %in% c("expanding", "fixed")) {
    stop ("The training set size must be either 'expanding' or 'fixed'!")
  }
  if (!backtesting_opt$use_backtesting) {
    if (backtesting_opt$backtesting_nb_iters != 1) {
      backtesting_opt$backtesting_nb_iters <- 1
      warning("As no backtesting is applied, there will be only one forecasting exercise for each time series!")
    }
  }
  return(backtesting_opt)
}

#' Check forecasting horizon
#' This function ensures that the selected forecasting horizon is valid.
#' If not, the function throws an error.
#'
#' @param fc_horizon must be a positive integer
#' @return fc_horizon: a positive integer
check_fc_horizon <- function(fc_horizon) {
  if (!base::is.numeric(fc_horizon)) {
    stop("The forecasting horizon must be an positive non-zero integer!")
  } else if (fc_horizon != base::as.integer(fc_horizon) | fc_horizon < 1) {
    stop("The forecasting horizon must be an positive non-zero integer!")
  }
  return(fc_horizon)
}

#' Check validation set size
#' This function ensures that the selected validation set size is valid.
#' If not, the function throws an error.
#'
#' @param valid_set_size must be a positive integer
#' @return valid_set_size: a positive integer
check_valid_set_size <- function(valid_set_size) {
  if (!base::is.numeric(valid_set_size)) {
    stop("The validation set size must be an positive non-zero integer!")
  } else if (valid_set_size != as.integer(valid_set_size) | valid_set_size < 0) {
    stop("The validation set size must be an positive non-zero integer!")
  }
  return(valid_set_size)
}

#' Check optional test set size
#' This function ensures that the optional test set size is valid. The optional
#' test set size is used to identify the best performing model inside the
#' h2o.automl procedure. If the argument is invalid, the function throws an error.
#'
#' @param tmp_test_set_size must be a positive integer
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
#' This function ensures that the backtesting iteration number is valid.
#' If not, the function throws an error.
#'
#' @param bt_iter must be a positive integer which is smaller or equal to the
#'     number of backtesting operations to perform.
#' @return bt_iter: a positive integer
check_backtesting_iter <- function(bt_iter, backtesting_opt = NULL) {
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  if (!base::is.numeric(bt_iter)) {
    stop("The sample number must be an positive non-zero integer!")
  } else if (bt_iter != base::as.integer(bt_iter) | bt_iter < 1) {
    stop("The sample number must be a positive non-zero integer!")
  } else if (bt_iter > backtesting_opt$backtesting_nb_iters) {
    stop("More iterations performed than specified in the backtesting plan!")
  }
  return(bt_iter)
}

#' Check the time series data and convert to xts
#' This function ensures that the input data is of type xts, ts or mts and
#' converts it to an xts object. Furthermore, if no colnames are specified, default
#' colnames will be applied. Moreover, punctuations in colnames will dropped to
#' avoid errors when converting between different data types.
#'
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
#' This function ensures that the filepath where forecast can be saved is valid
#' If not the argument is invalid, the function throws an error.
#'
#' @param save_fc_to_file NULL or a valid filepath
#' @return NULL or a valid filepath
check_save_fc_to_file <- function(save_fc_to_file) {
  if (!base::is.null(save_fc_to_file)) {
  }
  return(save_fc_to_file)
}

#' Check model names
#' This function ensures that the selected model names are valid.
#' If not, the function throws an error.
#'
#' @param model_names list or vector of strings
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
#' This function ensures that the models' arguments are a list. Furthermore, the
#' function checks if these arguments match the selected model names, otherwise they
#' will be dropped.
#'
#' @param models_args a list
#' @return models_args: a list
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

#' Check parallel processing option
#' This function ensures that the user selects a valid parallel processing option.
#' If use_parallel = True, then time series will be processed in parallel.
#'
#' @param use_parallel a boolean
#' @return a boolean
check_use_parallel <- function(use_parallel) {
  if (base::is.null(use_parallel)) {
    use_parallel = FALSE
  } else if (!base::is.logical(use_parallel)) {
    stop(base::paste("To use sequential or parallel processing, please specify FALSE (for seq. proc.)",
                     "or TRUE (for parallel proc.). By default, sequential proc. is applied."))
  }
  return(use_parallel)
}
