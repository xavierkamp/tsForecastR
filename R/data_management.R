#' Print to console the model name currently selected
#' @param model_name A string
print_model_name <- function(model_name) {
  model_name <- check_model_names(model_name)
  cat(paste("Currently forecasting with: ",
            model_name,
            "\n",
            sep = ""))
}

#' Extract forecasts and prediction intervals from list
#' @description
#' This function extracts the forecasts and prediction intervals from a list.
#' @param fc_obj A list, the forecasts must be stored under the keyword 'mean'.
#' @param exclude_PI A boolean, extract prediction intervals stored under the keywords 'upper' and 'lower'.
#' @return A data.frame
get_fc_with_PI <- function(fc_obj, exclude_PI = FALSE) {
  `%>%` <- magrittr::`%>%`
  if (!is.list(fc_obj)) {
    stop(paste("Input data must be a list! To save forecasts, ",
               "forecasts must be passed as a data.frame in a list under the keyword 'mean'!",
               sep = ""))
  } else {
    if (!"mean" %in% names(fc_obj)) {
      stop("Keyword 'mean' not found in list!")
    } else if (!is.data.frame(fc_obj$mean) & !is.ts(fc_obj$mean) & !is.numeric(fc_obj$mean)) {
      stop("Object stored as 'mean' in list is not a data.frame, ts obj or numeric!")
    }
  }
  if (!is.logical(exclude_PI)) {
    warning("Argument to exclude prediction interval is invalid, using FALSE as default!")
    exclude_PI <- FALSE
  }
  if (exclude_PI) {
    fc <-
      fc_obj$mean %>%
      as.data.frame() %>%
      dplyr::mutate(key = "predict")
    colnames(fc) <- c("values", "key")
  } else {
    fc <-
      cbind(fc_obj$mean,
            fc_obj$lower,
            fc_obj$upper) %>%
      as.data.frame() %>%
      dplyr::mutate(key = "predict")
    colnames(fc) <-
      c("values",
        paste("lower", colnames(fc_obj$lower), sep = "_"),
        paste("upper", colnames(fc_obj$upper), sep = "_"),
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
    paste(names(.), ., collapse = ";")
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
    as.data.frame() %>%
    dplyr::select("values" = colnames(.)) %>%
    dplyr::mutate(dates = zoo::index(data_xts) %>%
                    lubridate::as_date()) %>%
    dplyr::mutate(key = "actual")
  return(data_formated)
}

#' Combine forecasting info
#' @description
#' This function combines every info which will be stored in a data table.
#' @param model_name A string
#' @param fc_formated A data.frame
#' @param actual_formated A data.frame
#' @param split_keys A data.frame
#' @param model_descr A string
#' @param model_par A string
#' @param model_args A string
#' @return A data.frame object
combine_fc_results <- function(model_name,
                               fc_formated,
                               actual_formated,
                               split_keys,
                               model_descr = NULL,
                               model_par = NULL,
                               model_args = NULL) {
  `%>%` <- magrittr::`%>%`
  data_join <-
    dplyr::bind_rows(actual_formated,
                     fc_formated)  %>%
    dplyr::full_join(split_keys, by = "dates")

  results <-
    data_join %>%
    dplyr::bind_cols(model = model_name %>% rep(., nrow(data_join)),
                     .,
                     model_descr = model_descr %>% rep(., nrow(data_join)),
                     model_par = model_par %>% rep(., nrow(data_join)),
                     model_args = model_args %>% rep(., nrow(data_join)))
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
        as.data.frame() %>%
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
      if (length(.) != 0) {
        names(.) <- paste("coef.", names(.), sep = "")
        .
      }
    }
  model_coef_2 <-
    fc_obj$model$var.coef %>%
    {
      if (length(.) != 0) {
        var_name <- NULL
        for (row_name in rownames(.)) {
          for (col_name in colnames(.)) {
            var_name <- c(var_name,
                          paste("var.",
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
    paste(collapse = " ") %>%
    {
      names(.) <- "msg"
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
          if (!is.null(.)) {
          names(.) <- "lambda"
          }
          .
        },
      fc_obj$model$alpha %>%
        {
          if (!is.null(.)) {
          names(.) <- "alpha"
          }
          .
        },
      fc_obj$model$beta %>%
        {
          if (!is.null(.)) {
          names(.) <- "beta"
          }
          .
        },
      fc_obj$model$damping.parameter %>%
        {
          if (!is.null(.)) {
          names(.) <- "damping.parameter"
          }
          .
        },
      fc_obj$model$gamma.one.values %>%
        {
          if (!is.null(.)) {
            names(.) <- paste("gamma.one.values",
                              seq(nrow(.)),
                              sep = "_")
          }
          .
        },
      fc_obj$model$gamma.two.values %>%
        {
          if (!is.null(.)) {
            names(.) <- paste("gamma.two.values",
                              seq(nrow(.)),
                              sep = "_")
          }
          .
        },
      fc_obj$model$ar.coefficients %>%
        {
          if (!is.null(.)) {
            names(.) <- paste("ar.coefficients",
                              seq(nrow(.)),
                              sep = "_")
          }
          .
        },
      fc_obj$model$ma.coefficients %>%
        {
          if (!is.null(.)) {
            names(.) <- paste("ma.coefficients",
                              seq(nrow(.)),
                              sep = "_")
          }
          .
        },
      fc_obj$model$optim.return.code %>%
        {
          names(.) <- "optim.return.code"
          .
        },
      fc_obj$model$seed.states %>%
        {
          if (!is.null(.)) {
            names(.) <- paste("seed.states",
                              seq(nrow(.)),
                              sep = "_")
          }
          .
        },
      fc_obj$model$seasonal.periods %>%
        {
          if (!is.null(.)) {
          names(.) <- "seasonal.periods"
          }
          .
        },
      fc_obj$model$k.vector %>%
        {
          if (!is.null(.)) {
            names(.) <- "k.vector"
          }
          .
        },
      fc_obj$model$p %>%
        {
          if (!is.null(.)) {
          names(.) <- "p"
          }
          .
        },
      fc_obj$model$q %>%
        {
          if (!is.null(.)) {
          names(.) <- "q"
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
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param model_args A list, optional arguments to pass to the models
#' @param exclude_PI A boolean, exclude prediction intervals in results
#' @return A data frame
save_fc_forecast <- function(fc_obj, raw_data, sample_split,
                             save_fc_to_file, model_name,
                             model_args = NULL,
                             exclude_PI = FALSE, ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "forecast") {
    stop("forecasts must be a forecast object")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_name <- check_model_names(model_name)
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
  results <- combine_fc_results(model_name,
                                fc_formated,
                                raw_data_formated,
                                split_keys,
                                model_descr,
                                model_par,
                                model_args)
  if (!is.null(save_fc_to_file)) {
    file_name <- paste(save_fc_to_file,
                       colnames(ts_data_xts),
                       sep = "/")
    write.table(results,
                file = file_name,
                append = TRUE,
                eol = "\r\n",
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE)
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
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param model_args A list, optional arguments to pass to the models
#' @return A data frame
save_fc_bsts <- function(fc_obj, raw_data, sample_split,
                         save_fc_to_file, model_name,
                         model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "bsts.prediction") {
    stop("forecasts must be a bsts.prediction object")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_name <- check_model_names(model_name)
  pred_dates <-
    sample_split[["test"]] %>%
    zoo::index() %>%
    lubridate::as_date()
  pred_int <-
    fc_obj$interval %>%
    {
      rownames(.) <- c(paste("lower", rownames(.)[1], sep = "_"),
                       paste("upper", rownames(.)[1], sep = "_"))
      .
    } %>%
    t()
  fc_formated <-
    get_fc_with_PI(fc_obj, exclude_PI = TRUE) %>%
    dplyr::mutate(dates = pred_dates) %>%
    cbind(., pred_int)
  raw_data_formated <- format_historical_data(raw_data_xts)
  split_keys <- get_split_keys(sample_split)
  model_args <- collapse_model_par(model_args)
  results <- combine_fc_results(model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = raw_data_formated,
                                split_keys = split_keys,
                                model_args = model_args)
  if (!is.null(save_fc_to_file)) {
    file_name <- paste(save_fc_to_file,
                       colnames(ts_data_xts),
                       sep = "/")
    write.table(results,
                file = file_name,
                append = TRUE,
                eol = "\r\n",
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE)
    return(base::data.base())
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
#' @param save_fc_to_file A string, directory to which results can be saved as text files
#' @param model_name A string, name of the forecasting model
#' @param model_args A list, optional arguments to pass to the models
#' @return A data frame
save_fc_ml <- function(fc_obj, raw_data, sample_split,
                       save_fc_to_file, model_name,
                       model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (!is.data.frame(fc_obj)) {
    stop("Forecasts must be passed as a data.frame object!")
  }
  raw_data_xts <- check_data_sv_as_xts(raw_data)
  save_fc_to_file <- check_save_fc_to_file(save_fc_to_file)
  model_name <- check_model_names(model_name)
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
  results <- combine_fc_results(model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = raw_data_formated,
                                split_keys = split_keys,
                                model_args = model_args)
  if (!is.null(save_fc_to_file)) {
    file_name <- paste(save_fc_to_file,
                       colnames(ts_data_xts),
                       sep = "/")
    write.table(results,
                file = file_name,
                append = TRUE,
                eol = "\r\n",
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE)
    return(base::data.base())
  } else {
    return(results)
  }
}
