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
collapse_model_par <- function(model_par_vector) {
  `%>%` <- magrittr::`%>%`
  model_par <-
    model_par_vector %>%
    paste(names(.), ., collapse = ";")
  return(model_par)
}
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
extract_coef_arima <- function(fc_obj) {
  `%>%` <- magrittr::`%>%`
  model_coef_1 <-
    fc_obj$model$coef %>%
    {
      names(.) <- paste("coef.", names(.), sep = "")
      .
    }
  model_coef_2 <-
    fc_obj$model$var.coef %>%
    {
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
  model_coef <- c(model_coef_1, model_coef_2)
  return(model_coef)
}
extract_coef_ets <- function(fc_obj) {
  return(fc_obj$model$par)
}
extract_coef_stl <- function(fc_obj) {
  return(fc_obj$model$par)
}
extract_coef_snaive <- function(fc_obj) {
  return(fc_obj$model$par)
}
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
save_fc_forecast <- function(fc_obj, actual_data, sample_split,
                             save_fc_to_file, model_name,
                             model_args = NULL,
                             exclude_PI = FALSE, ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "forecast") {
    stop("forecasts must be a forecast object")
  }
  actual_data_xts <- check_data_sv_as_xts(actual_data)
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
  actual_data_formated <- format_historical_data(actual_data_xts)
  split_keys <- get_split_keys(sample_split)
  results <- combine_fc_results(model_name,
                                fc_formated,
                                actual_data_formated,
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
    return(NULL)
  } else {
    return(results)
  }
}
save_fc_bsts <- function(fc_obj, actual_data, sample_split,
                         save_fc_to_file, model_name,
                         model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (class(fc_obj)[1] != "bsts.prediction") {
    stop("forecasts must be a bsts.prediction object")
  }
  actual_data_xts <- check_data_sv_as_xts(actual_data)
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
  actual_data_formated <- format_historical_data(actual_data_xts)
  split_keys <- get_split_keys(sample_split)
  model_args <- collapse_model_par(model_args)
  results <- combine_fc_results(model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = actual_data_formated,
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
    return(NULL)
  } else {
    return(results)
  }
}
save_fc_lstm <- function(fc_obj, actual_data, sample_split,
                         save_fc_to_file, model_name,
                         model_args = NULL,
                         ...) {
  `%>%` <- magrittr::`%>%`
  if (!is.data.frame(fc_obj)) {
    stop("Forecasts must be passed as a data.frame object!")
  }
  actual_data_xts <- check_data_sv_as_xts(actual_data)
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
  actual_data_formated <- format_historical_data(actual_data_xts)
  split_keys <- get_split_keys(sample_split)
  model_args <- collapse_model_par(model_args)
  results <- combine_fc_results(model_name = model_name,
                                fc_formated = fc_formated,
                                actual_formated = actual_data_formated,
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
    return(NULL)
  } else {
    return(results)
  }
}
