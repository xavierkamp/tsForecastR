#' Print to console the model name currently selected
#' @param model_name A string
print_model_name <- function(model_name) {
  model_name <- check_model_names(model_name)
  cat(base::paste("Currently forecasting with: ",
                  model_name,
                  "\n",
                  sep = ""))
}

#' Initialize the model output
#' @description
#' Generate an empty list with a tsForecastR class attribute
ini_model_output <- function() {
  return(structure(base::list(),
                   class = "tsForecastR"))
}

#' Extract forecasts and prediction intervals from list
#' @description
#' This function extracts the forecasts and prediction intervals from a list.
#' @param fc_obj A list, the forecasts must be stored under the keyword 'mean'.
#' @param exclude_PI A boolean, extract prediction intervals stored under the keywords 'upper' and 'lower'.
#' @return A data.frame
get_fc_with_PI <- function(fc_obj, exclude_PI = FALSE) {
  `%>%` <- magrittr::`%>%`
  if (!base::is.list(fc_obj)) {
    stop(base::paste("Input data must be a list! To save forecasts, ",
                     "forecasts must be passed as a data.frame in a list under the keyword 'mean'!",
                     sep = ""))
  } else {
    if (!"mean" %in% base::names(fc_obj)) {
      stop("Keyword 'mean' not found in list!")
    } else if (!base::is.data.frame(fc_obj$mean)
               & !stats::is.ts(fc_obj$mean)
               & !base::is.numeric(fc_obj$mean)) {
      stop("Object stored as 'mean' in list is not a data.frame, ts obj or numeric!")
    }
  }
  if (!base::is.logical(exclude_PI)) {
    message("Argument to exclude prediction interval is invalid, using FALSE as default!")
    exclude_PI <- FALSE
  }
  if (exclude_PI) {
    fc <-
      fc_obj$mean %>%
      base::as.data.frame() %>%
      dplyr::mutate(key = "predict")
    base::colnames(fc) <- c("values", "key")
  } else {
    fc <-
      cbind(fc_obj$mean,
            fc_obj$lower,
            fc_obj$upper) %>%
      base::as.data.frame() %>%
      dplyr::mutate(key = "predict")
    base::colnames(fc) <-
      c("values",
        base::paste("lower", base::colnames(fc_obj$lower), sep = "_"),
        base::paste("upper", base::colnames(fc_obj$upper), sep = "_"),
        "key")
  }
  return(fc)
}

#' Store model estimates as a string
#' @description
#' This function collapses a vector to a single string where values are separated by ';'. This conversion
#' is useful when estimates are later stored in a data table, providing the user the possibility to check on
#' the most important model parameters.
#' @param model_par_vector A vector, estimates of the model parameters to be converted to a single string.
#' @return A string
collapse_model_par <- function(model_par_vector) {
  `%>%` <- magrittr::`%>%`
  model_par <-
    model_par_vector %>%
    base::paste(base::names(.), ., collapse = ";")
  return(model_par)
}

#' Format original data
#' @description
#' This function ensures that the original data is uniformely formatted across all forecasting procedures
#' before saving it in a data table. The original data can be accessed under 'values' with 'key'=='actual'.
#' @param data_xts An xts object, the original (i.e. unprocessed) time series data
#' @return A data.frame object
format_historical_data <- function(data_xts) {
  `%>%` <- magrittr::`%>%`
  data_formated <-
    data_xts %>%
    base::as.data.frame() %>%
    dplyr::select("values" = base::colnames(.)) %>%
    dplyr::mutate(dates = zoo::index(data_xts) %>%
                    lubridate::as_date()) %>%
    dplyr::mutate(key = "actual")
  return(data_formated)
}

#' Combine forecasting info
#' @description
#' This function combines every info which will be stored in a data table.
#' @param ts_name A string
#' @param model_name A string
#' @param fc_formated A data.frame
#' @param actual_formated A data.frame
#' @param split_keys A data.frame
#' @param model_descr A string
#' @param model_par A string
#' @param model_args A string
#' @param period_iter A string, period identifier of format: 'period' + '_' + iter
#' @param time_id A POSIXct, created with \code{\link[base]{Sys.time}} and appended to results
#' @param ... Additional arguments to be passed to the function
#' @return A data.frame object
combine_fc_results <- function(ts_name,
                               model_name,
                               fc_formated,
                               actual_formated,
                               split_keys,
                               model_descr = NULL,
                               model_par = NULL,
                               model_args = NULL,
                               period_iter = NULL,
                               time_id = NULL,
                               ...) {
  `%>%` <- magrittr::`%>%`
  data_join <-
    dplyr::bind_rows(actual_formated,
                     fc_formated)  %>%
    dplyr::full_join(split_keys, by = "dates")

  results <-
    data_join %>%
    dplyr::bind_cols(ts_name = ts_name %>% base::rep(., base::nrow(data_join)),
                     period = period_iter %>% base::rep(., base::nrow(data_join)),
                     model = model_name %>% base::rep(., base::nrow(data_join)),
                     .,
                     model_descr = model_descr %>% base::rep(., base::nrow(data_join)),
                     model_par = model_par %>% base::rep(., base::nrow(data_join)),
                     model_args = model_args %>% base::rep(., base::nrow(data_join)),
                     time_id = time_id %>% base::rep(., base::nrow(data_join)))
  return(results)
}

#' Get split identifiers
#' @description
#' This function identifies which observations belong to which set (e.g. training, validation, test sets) by creating
#' a split identifier.
#' @param sample_split A list, the sample split
#' @return A data.frame
get_split_keys <- function(sample_split) {
  `%>%` <- magrittr::`%>%`
  names_split <- names(sample_split)
  sample_split_df <- NULL
  for (split in names_split) {
    sample_split_df <-
      dplyr::bind_rows(
        sample_split_df,
      sample_split[[split]] %>%
        base::as.data.frame() %>%
        dplyr::mutate(dates = sample_split[[split]] %>%
                        zoo::index() %>%
                        lubridate::as_date())%>%
        dplyr::mutate("split_key" = split)
      )
  }
  data <-
    sample_split_df %>%
    dplyr::select(dates, split_key)
  return(data)
}

#' Extract model estimates for ARIMA
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_arima <- function(fc_obj) {
  `%>%` <- magrittr::`%>%`
  model_coef_1 <-
    fc_obj$model$coef %>%
    {
      if (base::length(.) != 0) {
        base::names(.) <- base::paste("coef.", base::names(.), sep = "")
        .
      }
    }
  model_coef_2 <-
    fc_obj$model$var.coef %>%
    {
      if (base::length(.) != 0) {
        var_name <- NULL
        for (row_name in base::rownames(.)) {
          for (col_name in base::colnames(.)) {
            var_name <- c(var_name,
                          base::paste("var.",
                                      row_name,
                                      ".",
                                      col_name,
                                      sep = ""))
          }
        }
        vectorized_data <- c(.)
        names(vectorized_data) <- var_name
        vectorized_data
      }
    }
  model_coef <- c(model_coef_1, model_coef_2)
  return(model_coef)
}

#' Extract model estimates for ETS
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_ets <- function(fc_obj) {
  return(fc_obj$model$par)
}

#' Extract model estimates for STL
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_stl <- function(fc_obj) {
  return(fc_obj$model$par)
}

#' Extract model estimates for seasonal naive
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_snaive <- function(fc_obj) {
  return(fc_obj$model$par)
}

#' Extract model estimates for NNETAR
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_nnetar <- function(fc_obj) {
  `%>%` <- magrittr::`%>%`
  model_coef <-
    capture.output(fc_obj$model) %>%
    base::paste(collapse = " ") %>%
    {
      base::names(.) <- "msg"
      .
    }
  return(model_coef)
}

#' Extract model estimates for TBATS
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @return A vector of strings
extract_coef_tbats <- function(fc_obj) {
  `%>%` <- magrittr::`%>%`
  model_coef <-
    c(
      fc_obj$model$lambda %>%
        {
          if (!base::is.null(.)) {
          names(.) <- "lambda"
          }
          .
        },
      fc_obj$model$alpha %>%
        {
          if (!base::is.null(.)) {
          names(.) <- "alpha"
          }
          .
        },
      fc_obj$model$beta %>%
        {
          if (!base::is.null(.)) {
          names(.) <- "beta"
          }
          .
        },
      fc_obj$model$damping.parameter %>%
        {
          if (!base::is.null(.)) {
          names(.) <- "damping.parameter"
          }
          .
        },
      fc_obj$model$gamma.one.values %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- base::paste("gamma.one.values",
                                          base::seq(base::nrow(.)),
                                          sep = "_")
          }
          .
        },
      fc_obj$model$gamma.two.values %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- base::paste("gamma.two.values",
                                          base::seq(base::nrow(.)),
                                          sep = "_")
          }
          .
        },
      fc_obj$model$ar.coefficients %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- base::paste("ar.coefficients",
                                          base::seq(base::nrow(.)),
                                          sep = "_")
          }
          .
        },
      fc_obj$model$ma.coefficients %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- base::paste("ma.coefficients",
                                          base::seq(base::nrow(.)),
                                          sep = "_")
          }
          .
        },
      fc_obj$model$optim.return.code %>%
        {
          base::names(.) <- "optim.return.code"
          .
        },
      fc_obj$model$seed.states %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- base::paste("seed.states",
                                          base::seq(base::nrow(.)),
                                          sep = "_")
          }
          .
        },
      fc_obj$model$seasonal.periods %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- "seasonal.periods"
          }
          .
        },
      fc_obj$model$k.vector %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- "k.vector"
          }
          .
        },
      fc_obj$model$p %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- "p"
          }
          .
        },
      fc_obj$model$q %>%
        {
          if (!base::is.null(.)) {
            base::names(.) <- "q"
          }
          .
        })
  return(model_coef)
}

#' Save forecasts (for forecast objects)
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A forecast object
#' @param raw_data A univariate ts or xts object, original (i.e. unprocessed) time series data
#' @param sample_split A list, the sample split
#' @param data_dir A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param time_id A POSIXct, created with \code{\link[base]{Sys.time}} and appended to results
#' @param period_iter A string, period identifier of format: 'period' + '_' + iter
#' @param model_args A list, optional arguments to pass to the models
#' @param exclude_PI A boolean, exclude prediction intervals in results
#' @param ... Additional arguments to be passed to the function
#' @return A data frame
save_fc_forecast <- function(fc_obj, raw_data, sample_split,
                             data_dir, model_name,
                             time_id = base::Sys.time(),
                             period_iter = NULL,
                             model_args = NULL,
                             exclude_PI = FALSE, ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "forecast") {
    stop("forecasts must be a forecast object")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  data_dir <- check_data_dir(data_dir)
  model_name <- check_model_names(model_name)
  time_id <- check_time_id(time_id)
  period_iter <- check_period_iter(period_iter)
  pred_dates <-
    sample_split[["test"]] %>%
    zoo::index() %>%
    lubridate::as_date()
  fc_formated <-
    get_fc_with_PI(fc_obj, exclude_PI) %>%
    dplyr::mutate(dates = pred_dates)
  eval(parse(text = paste("model_coef <- ",
                          "extract_coef_",
                          model_name,
                          "(fc_obj)",
                          sep = "")))
  model_par <- collapse_model_par(model_coef)
  model_args <- collapse_model_par(model_args)
  model_descr <- fc_obj$method
  raw_data_formated <- format_historical_data(raw_data_xts)
  split_keys <- get_split_keys(sample_split)
  results <- combine_fc_results(ts_name = base::colnames(raw_data_xts),
                                model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = raw_data_formated,
                                split_keys = split_keys,
                                model_descr = model_descr,
                                model_par = model_par,
                                model_args = model_args,
                                period_iter = period_iter,
                                time_id = time_id)
  if (!base::is.null(data_dir)) {
    if (period_iter == "period_1") {
      append_colnames <- TRUE
    } else {
      append_colnames <- FALSE
    }
    file_name <- base::paste(data_dir,
                             base::paste(base::colnames(raw_data_xts),
                                         model_name,
                                         sep = "_"),
                             sep = "/")
    suppressWarnings(write.table(results,
                                 file = file_name,
                                 append = TRUE,
                                 eol = "\r\n",
                                 sep = "\t",
                                 col.names = append_colnames,
                                 row.names = FALSE))
    return(base::data.frame())
  } else {
    return(results)
  }
}

#' Save forecasts (for bsts.prediction objects)
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A bsts.prediction object
#' @param raw_data A univariate ts or xts object, original (i.e. unprocessed) time series data
#' @param sample_split A list, the sample split
#' @param data_dir A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param time_id A POSIXct, created with \code{\link[base]{Sys.time}} and appended to results
#' @param period_iter A string, period identifier of format: 'period' + '_' + iter
#' @param model_args A list, optional arguments to pass to the models
#' @param ... Additional arguments to be passed to the function
#' @return A data frame
save_fc_bsts <- function(fc_obj, raw_data, sample_split,
                         data_dir, model_name,
                         time_id = base::Sys.time(),
                         period_iter = NULL,
                         model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "bsts.prediction") {
    stop("forecasts must be a bsts.prediction object")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  data_dir <- check_data_dir(data_dir)
  model_name <- check_model_names(model_name)
  time_id <- check_time_id(time_id)
  period_iter <- check_period_iter(period_iter)
  pred_dates <-
    sample_split[["test"]] %>%
    zoo::index() %>%
    lubridate::as_date()
  pred_int <-
    fc_obj$interval %>%
    {
      base::colnames(.) <- c(base::paste("lower", base::colnames(.)[1], sep = "_"),
                             base::paste("upper", base::colnames(.)[1], sep = "_"))
      .
    } %>%
    as.data.frame()
  fc_formated <-
    get_fc_with_PI(fc_obj, exclude_PI = TRUE) %>%
    dplyr::mutate(dates = pred_dates) %>%
    base::cbind(., pred_int)
  raw_data_formated <- format_historical_data(raw_data_xts)
  split_keys <- get_split_keys(sample_split)
  model_args <- collapse_model_par(model_args)
  results <- combine_fc_results(ts_name = base::colnames(raw_data_xts),
                                model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = raw_data_formated,
                                split_keys = split_keys,
                                model_args = model_args,
                                period_iter = period_iter,
                                time_id = time_id)
  if (!is.null(data_dir)) {
    if (period_iter == "period_1") {
      append_colnames <- TRUE
    } else {
      append_colnames <- FALSE
    }
    file_name <- base::paste(data_dir,
                             base::paste(base::colnames(raw_data_xts),
                                         model_name,
                                         sep = "_"),
                             sep = "/")
    suppressWarnings(write.table(results,
                                 file = file_name,
                                 append = TRUE,
                                 eol = "\r\n",
                                 sep = "\t",
                                 col.names = append_colnames,
                                 row.names = FALSE))
    return(base::data.frame())
  } else {
    return(results)
  }
}

#' Save forecasts (for forecast objects)
#' @description
#' This function extracts the estimates of the model parameters
#' @param fc_obj A data.frame object
#' @param raw_data A univariate ts or xts object, original (i.e. unprocessed) time series data
#' @param sample_split A list, the sample split
#' @param data_dir A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param time_id A POSIXct, created with \code{\link[base]{Sys.time}} and appended to results
#' @param period_iter A string, period identifier of format: 'period' + '_' + iter
#' @param model_args A list, optional arguments to pass to the models
#' @param ... Additional arguments to be passed to the function
#' @return A data frame
save_fc_ml <- function(fc_obj, raw_data, sample_split,
                       data_dir, model_name,
                       time_id = base::Sys.time(),
                       period_iter = NULL,
                       model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (!base::is.data.frame(fc_obj)) {
    stop("Forecasts must be passed as an xts object!")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  data_dir <- check_data_dir(data_dir)
  model_name <- check_model_names(model_name)
  time_id <- check_time_id(time_id)
  period_iter <- check_period_iter(period_iter)
  pred_dates <-
    sample_split[["test"]] %>%
    zoo::index() %>%
    lubridate::as_date()
  fc_list <- list()
  fc_list$mean <- fc_obj
  fc_formated <-
    get_fc_with_PI(fc_list, exclude_PI = TRUE) %>%
    dplyr::mutate(dates = pred_dates)
  raw_data_formated <- format_historical_data(raw_data_xts)
  split_keys <- get_split_keys(sample_split)
  model_args <- collapse_model_par(model_args)
  results <- combine_fc_results(ts_name = base::colnames(raw_data_xts),
                                model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = raw_data_formated,
                                split_keys = split_keys,
                                model_args = model_args,
                                period_iter = period_iter,
                                time_id = time_id)
  if (!base::is.null(data_dir)) {
    if (period_iter == "period_1") {
      append_colnames <- TRUE
    } else {
      append_colnames <- FALSE
    }
    file_name <- base::paste(data_dir,
                             base::paste(base::colnames(raw_data_xts),
                                         model_name,
                                         sep = "_"),
                             sep = "/")
    suppressWarnings(utils::write.table(results,
                                        file = file_name,
                                        append = TRUE,
                                        eol = "\r\n",
                                        sep = "\t",
                                        col.names = append_colnames,
                                        row.names = FALSE))
    return(base::data.frame())
  } else {
    return(results)
  }
}

#' Recursive function to read results from tsForecastR object
#' @param fc A tsForecastR object
#' @return A data frame
read_tsForecastR <- function(fc) {
  `%>%` <- magrittr::`%>%`
  df <- base::data.frame()
  if (base::is.data.frame(fc)) {
    return(fc)
  } else if (!is.null(base::names(fc))) {
    for (i in base::names(fc)) {
      df <-
        dplyr::bind_rows(df,
                         base::paste("fc[['",
                                     i,
                                     "']]",
                                     sep = "") %>%
                           base::parse(text = .) %>%
                           base::eval(.) %>%
                           read_tsForecastR(.))
    }
  }
  return(df)
}

#' Read results from files
#' @param data_colnames a vector of strings, colnames to be cleaned
#' @param data_dir A string, directory to which results can be saved as text files
#' @param model_names A list or vector of strings representing the model names to be used
#' @return A data frame
read_fc_from_file <- function(data_colnames,
                              data_dir,
                              model_names) {
  `%>%` <- magrittr::`%>%`
  data_dir <- check_data_dir(data_dir)
  data_colnames <- check_colnames(data_colnames)
  df <- base::data.frame()
  for (ts_name in data_colnames) {
    for (method in model_names) {
      suppressWarnings(
        base::tryCatch({
          file_name <- base::paste(data_dir,
                                   base::paste(ts_name, method, sep = "_"),
                                   sep = "/")
          file_data <-
            read.table(file_name,
                       header = TRUE,
                       sep = "\t") %>%
            dplyr::filter(ts_name %in% data_colnames)
          df <- dplyr::bind_rows(df, file_data)
        }, error = function(e) {
          message(base::paste("File named '", file_name, "' not found",
                              sep = ""))
        }))
    }
  }
  return(df)
}

#' Read forecasts from tsForecastR object
#' @description This function transforms a tsForecastR object into a data.frame object
#' @param fc A tsForecastR object
#' @param data_dir A string, directory to which results can be saved as text files.
#' @param data_colnames A vector of strings, the names of the time series objects to read the
#' results from
#' @param model_names A vector of strings, the models to read the results from
#' @param ... Additional arguments to be passed to the function
#' @return A data frame
#' @examples
#' \dontrun{
#' library(datasets)
#'
#' fc <- generate_fc(AirPassengers)
#' df <- save_as_df(fc)
#' print(df)
#' }
#' @export
save_as_df <- function(fc = NULL,
                       data_dir = NULL,
                       data_colnames = NULL,
                       model_names = NULL,
                       ...) {
  `%>%` <- magrittr::`%>%`
  data_dir <- check_data_dir(data_dir)
  model_names <- check_model_names(model_names)
  if (base::is.null(data_dir)) {
    if (base::is.null(fc)) {
      stop(base::paste("No data found! Please specify a valid data directory and time series' ",
                       "names or specify a valid tsForecastR object.",
                       sep = ""))
    } else if (class(fc) != "tsForecastR") {
      stop("The value of the fc argument is invalid! Please specify a valid tsForecastR object.")
    }
  } else {
    if (base::is.null(data_colnames)) {
      message(base::paste("No time series' names found! When reading files from a data directory, ",
                          "the time series' names must be provided. Otherwise, no file can be read ",
                          "from the directory and data_dir will be set to NULL as default.",
                          sep = ""))
      data_dir <- NULL
      if (base::is.null(fc)) {
        stop(base::paste("No data found! Please specify a valid data directory and time series' ",
                         "names or specify a valid tsForecastR object.",
                         sep = ""))
      } else if (class(fc) != "tsForecastR") {
        stop(base::paste("No data found! Please specify a valid data directory and time series' ",
                         "names or specify a valid tsForecastR object.",
                         sep = ""))
      }
    } else {
      data_colnames <- check_colnames(data_colnames)
    }
  }
  if (base::is.null(data_dir)) {
    results_df <- read_tsForecastR(fc)
  } else {
    results_df <- read_fc_from_file(data_dir = data_dir,
                                    data_colnames = data_colnames,
                                    model_names = model_names)
  }
  return(results_df)
}
