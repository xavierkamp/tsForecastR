#' Add empty placeholders
#' @description Empty values are added when forecasts are generated for future periods (i.e. values are unknown)
#' @param ts_data A univariate 'ts' or 'xts' object
#' @param fc_horizon An integer, the forecasting horizon (i.e. the number of periods to forecast)
#' @param backtesting_opt A list, options which define the backtesting approach:
#'
#'  use_bt - A boolean, to determine whether forecasts should be generated on future dates (default) or on past values. Generating
#'  forecasts on past dates allows to measure past forecast accuracy and to monitor a statistical model's ability to learn
#'  signals from the data.
#'
#'  nb_iters - An integer, to determine the number of forecasting operations to apply (When no backtesting is selected, then only
#'  one forecasting exercise is performed)
#'
#'  method - A string, to determine whether to apply a 'rolling' (default) or a 'moving' forecasting window. When 'rolling' is selected,
#'  after each forecasting exercise, the forecasting interval increments by one period and drops the last period to include it in
#'  the new training sample. When 'moving' is selected, the forecasting interval increments by its size rather than one period.
#'
#'  sample_size - A string, to determine whether the training set size should be 'expanding' (default) or 'fixed'.
#'  When 'expanding' is selected, then after each forecasting operation, the periods dropped from the forecasting interval will
#'  be added to the training set. When 'fixed' is selected, then adding new periods to the training set will require dropping as
#'  many last periods to keep the set's size constant.
#'
#' @return A univariate 'xts' object
add_placeholders <- function(ts_data, fc_horizon, backtesting_opt = NULL) {
  `%>%` <- magrittr::`%>%`
  input_data_xts <- check_data_sv_as_xts(ts_data)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  if (backtesting_opt$use_bt) {
    placeholder_data_xts <- input_data_xts
  } else {
    future_dates <-
      timetk::tk_make_future_timeseries(zoo::index(input_data_xts),
                                        fc_horizon)
    placeholder_data_xts <-
      base::rbind(input_data_xts %>%
                    base::as.matrix(),
                  base::rep(NA, fc_horizon) %>%
                    base::matrix(ncol = 1)) %>%
      xts::xts(order.by = c(zoo::index(input_data_xts),
                       future_dates))
  }
  return(placeholder_data_xts)
}

#' Add features
#' @param mts_data A univariate or multivariate 'ts', 'mts' or 'xts' object
#' @param xreg_data A univariate or multivariate 'ts', 'mts' or 'xts' object, optional external regressors
#' @return A univariate or multivariate 'xts' object
add_features <- function(mts_data, xreg_data = NULL) {
  `%>%` <- magrittr::`%>%`
  input_data_xts <- check_data_sv_as_xts(mts_data)
  xreg_data_xts <- check_data_sv_as_xts(xreg_data)
  if (base::is.null(xreg_data_xts)) {
    joined_data_xts <- input_data_xts
  } else {
    shared_xreg <- !base::colnames(xreg_xts) %>% stringr::str_detect("__")
    specific_xreg <- base::colnames(xreg_xts) %>% stringr::str_detect(base::colnames(input_data_xts))
    valid_xreg <- (shared_xreg | specific_xreg)
    xreg_names <- base::colnames(xreg_xts)[valid_xreg]
    joined_data_xts <-
      input_data_xts %>%
      xts::merge.xts(xreg_xts[, xreg_names], join = "left")
  }
  return(joined_data_xts)
}

#' Split the data into a training, validation and test set
#' @param mts_data A univariate or multivariate 'ts', 'mts' or 'xts' object
#' @param fc_horizon An integer, the forecasting horizon (i.e. the number of periods to forecast)
#' @param bt_iter An integer, number of the current backtesting operation (i.e. forecasting exercise). This argument
#' must be smaller or equal to the number of backtesting operations to perform.
#'
#' @param valid_set_size An integer, the validation set size (default = 0)
#' @param tmp_test_set_size An integer, size of a second test set (used by h2o.automl) (default = 0)
#' @param backtesting_opt A list, options which define the backtesting approach:
#'
#'  use_bt - A boolean, to determine whether forecasts should be generated on future dates (default) or on past values. Generating
#'  forecasts on past dates allows to measure past forecast accuracy and to monitor a statistical model's ability to learn
#'  signals from the data.
#'
#'  nb_iters - An integer, to determine the number of forecasting operations to apply (When no backtesting is selected, then only
#'  one forecasting exercise is performed)
#'
#'  method - A string, to determine whether to apply a 'rolling' (default) or a 'moving' forecasting window. When 'rolling' is selected,
#'  after each forecasting exercise, the forecasting interval increments by one period and drops the last period to include it in
#'  the new training sample. When 'moving' is selected, the forecasting interval increments by its size rather than one period.
#'
#'  sample_size - A string, to determine whether the training set size should be 'expanding' (default) or 'fixed'.
#'  When 'expanding' is selected, then after each forecasting operation, the periods dropped from the forecasting interval will
#'  be added to the training set. When 'fixed' is selected, then adding new periods to the training set will require dropping as
#'  many last periods to keep the set's size constant.
#'
#' @param ... Additional arguments to be passed to the function
#' @return A list with training, validation and test sets
split_train_test_set <- function(mts_data, fc_horizon = 12, bt_iter = 1,
                                 valid_set_size = 0, tmp_test_set_size = 0,
                                 backtesting_opt = NULL,
                                 ...) {
  `%>%` <- magrittr::`%>%`
  split <- base::list()
  x_train <- x_valid <- x_tmp_test <- x_test <- NULL
  input_data_xts <- check_data_sv_as_xts(mts_data)
  fc_horizon <- check_fc_horizon(fc_horizon)
  valid_set_size <- check_valid_set_size(valid_set_size)
  tmp_test_set_size <- check_tmp_test_set_size(tmp_test_set_size)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  bt_iter <- check_backtesting_iter(bt_iter, backtesting_opt)
  backtesting_sample_size <- backtesting_opt$sample_size
  backtesting_method <- backtesting_opt$method
  nb_periods <- backtesting_opt$nb_iters
  base::tryCatch(
    {
      if (backtesting_opt$use_bt) {
        if ((backtesting_sample_size[1] == 'expanding') & (backtesting_method[1] == 'rolling')) {
          last_train_pos <-
            (base::nrow(input_data_xts)
             - fc_horizon
             - nb_periods
             + bt_iter
             - valid_set_size
             - tmp_test_set_size)
          x_train <- input_data_xts[1:(last_train_pos), ]
        } else if ((backtesting_sample_size[1] == 'expanding' & backtesting_method[1] == 'moving')) {
          last_train_pos <-
            (base::nrow(input_data_xts)
             - fc_horizon * (nb_periods - bt_iter + 1)
             - valid_set_size
             - tmp_test_set_size)
          x_train <- input_data_xts[1:(last_train_pos), ]
        } else if ((backtesting_sample_size[1] == 'fixed' & backtesting_method[1] == 'rolling')) {
          last_train_pos <-
            (base::nrow(input_data_xts)
             - fc_horizon
             - nb_periods
             + bt_iter
             - valid_set_size
             - tmp_test_set_size)
          x_train <- input_data_xts[(1 + bt_iter - 1):(last_train_pos), ]
        } else if ((backtesting_sample_size[1] == 'fixed' & backtesting_method[1] == 'moving')) {
          last_train_pos <-
            (base::nrow(input_data_xts)
             - fc_horizon * (nb_periods - bt_iter + 1)
             - valid_set_size
             - tmp_test_set_size)
          x_train <- input_data_xts[(1 + fc_horizon * (bt_iter - 1)):(last_train_pos), ]
        }
      } else {
        last_train_pos <-
          (base::nrow(input_data_xts)
           - valid_set_size
           - tmp_test_set_size
           - fc_horizon)
        x_train <- input_data_xts[1:(last_train_pos), ]
      }
      last_valid_pos <-
        (last_train_pos
         + valid_set_size)
      last_tmp_test_pos <-
        (last_valid_pos
         + tmp_test_set_size)
      if (valid_set_size == 0) {
        x_valid <- NULL
      } else {
        x_valid <- input_data_xts[(last_train_pos + 1):(last_valid_pos), ]
      }
      if (tmp_test_set_size == 0) {
        x_tmp_test <- NULL
      } else {
        x_tmp_test <- input_data_xts[(last_valid_pos + 1):(last_tmp_test_pos), ]
      }
      x_test <- input_data_xts[(last_tmp_test_pos + 1):(last_tmp_test_pos + fc_horizon), ]
    },
    error = function(e) {
      message(e)
    }
  )
  split <- base::list(x_train, x_valid, x_tmp_test, x_test)
  base::names(split) <- c("train", "valid", "tmp_test", "test")
  return(split)
}

#' Determine the number of differencing to obtain a stationary time series
#' @description Uses \code{\link[forecast]{nsdiffs}} and \code{\link[forecast]{ndiffs}}
#' to determine the number of differencing to obtain a stationary time series.
#' @param ts_data A univariate 'ts' or 'xts' object
#' @param ... Additional arguments to be passed to the function
#' @examples
#' \dontrun{
#' library(datasets)
#' nb_diffs(AirPassengers)
#' }
#' @return A list, training, validation and test sets
nb_diffs <- function(ts_data, ...) {
  `%>%` <- magrittr::`%>%`
  input_data <-
    check_data_sv_as_xts(ts_data) %>%
    timeSeries::na.contiguous() %>%
    stats::as.ts()
  ns_diffs <-
    tryCatch({
      forecast::nsdiffs(input_data, ...)
    }, error = function(e) {
      return(0)
    })
  ndiffs <-
    tryCatch({
      forecast::ndiffs(input_data, ...)
    }, error = function(e) {
      return(0)
    })
  list_diffs <- base::list(ns_diffs, ndiffs)
  base::names(list_diffs) <- c("ns_diffs", "ndiffs")
  return(list_diffs)
}

#' Customized preprocessing function
#' @description The user can specify a custom preprocessing function to deal with missing values
#' @param ts_data A univariate 'ts' or 'xts' object
#' @param transf_fct A function, function which transforms the data
#' @examples
#' \dontrun{
#' library(datasets)
#' library(timeSeries)
#' preprocessing(AirPassengers, timeSeries::na.contiguous)
#'
#' library(imputeTS)
#' preprocessing(AirPassengers, imputeTS::na.mean)
#' }
#' @return An xts object, the transformed data
preprocess_custom_fct <- function(ts_data, transf_fct = NULL) {
  `%>%` <- magrittr::`%>%`
  xts_data <- check_data_sv_as_xts(ts_data)
  transf_fct <- check_preprocess_fct(transf_fct)
  if (base::is.null(transf_fct)) {
    transformed_data <- xts_data
  } else if (base::is.function(transf_fct)) {
    transformed_data <-
      xts_data %>%
      transf_fct()
  } else if (base::is.list(transf_fct)) {
    custom_fct <- transf_fct[[1]]
    if (!base::is.function(custom_fct)) {
      message("First argument in list must be a function! Using no transformation by default")
      transformed_data <- xts_data
    } else {
      fct_args <- transf_fct[[-1]]
      transformed_data <-
        xts_data %>%
        base::do.call(., custom_fct, c(fct_args))
    }
  } else {
    message("Arguments were invalid. No transformation will be applied!")
    transformed_data <- xts_data
  }
  return(transformed_data)
}

#' Normalize the data
#' @param data_df A 'data.frame' object
#' @return A list, normalized data and scaling arguments
normalize_data <- function(data_df) {
  `%>%` <- magrittr::`%>%`
  if (!base::is.data.frame(data_df)) {
    stop("The data must be a data.frame obj before normalizing!")
  }
  df <- data_df
  features_to_norm <- base::colnames(df)[base::colnames(df) != "key"]
  scale_arg_ls <- list()
  for (feature in features_to_norm) {
    position_feature <-
      base::colnames(df) %>%
      purrr::detect_index(function(x) x == feature)
    tmp_data_df <-
      df %>%
      dplyr::mutate(tmp_var = df[, feature] %>%
                      base::unlist())
    filtered_data <- dplyr::filter(tmp_data_df, key == "Training")
    mean_history <- base::mean(filtered_data$tmp_var, na.rm = TRUE)
    scale_history <- base::ifelse(stats::sd(filtered_data$tmp_var, na.rm = TRUE) == 0,
                                  0.001,
                                  stats::sd(filtered_data$tmp_var))
    df <-
      tmp_data_df %>%
      dplyr::mutate(tmp_var = (tmp_var - mean_history)/scale_history) %>%
      {
        .[, position_feature] <- .[, "tmp_var"]
        .
      } %>%
      dplyr::select(-tmp_var)

    base::eval(base::parse(text = base::paste("scale_arg_ls$", feature, " <-
                                                 c(mean_history, scale_history) %>%
                                                 matrix(ncol = 2) %>%
                                                 {
                                                   colnames(.) <- c('mean_history',
                                                                    'scale_history')
                                                   .
                                                 }",
                                              sep = "")))
  }
  normalized_data <- base::list(df, scale_arg_ls)
  base::names(normalized_data) <- c("data", "scale_arg_ls")
  return(normalized_data)
}

#' Add timesteps
#' @description
#' For lstm, when the number of time steps (i.e. number of lags) is defined,
#' for each input variable, the required number of lagged values needs to be
#' added as inputs to the input matrix
#'
#' When the ts is seasonal with periodicity = 12, then lag_setting should be 12.
#' The volume of the same month of previous year will most likely have a high
#' influence on today's volume.
#' When the ts is non-seasonal, lag_setting = 1, i.e. the volume of previous
#' month will most likely have the strongest impact on today.
#'
#' When lag_setting = 12 and tsteps < lag_setting (e.g. tsteps=4),
#' then input values will be: t-12, t-11, t-10, t-9.
#' When lag_setting = 12 and tsteps = lag_setting, then input values will be:
#' t-12, t-11, ... , t-1.
#' When lag_setting = 12 and tsteps > lag_setting (e.g. tsteps = 15),
#' then input values will be: t-15, ... , t-12, ... , t-1.
#'
#' Same goes for lag_setting other than 12.
#' @param data_df A 'data.frame' object
#' @param fc_horizon An integer, the forecasting horizon (i.e. the number of periods to forecast)
#' @param valid_set_size An integer, the validation set size (default = 0)
#' @param tsteps An integer, the number of time steps (i.e. lags) with explanatory power. These will be included as regressors.
#' @param lag_setting An integer, the periodicity of the data. Important when dealing with seasonal data.
#' @param ... Additional arguments to be passed to the function
#' @return A data.frame object
add_timesteps <- function(data_df,
                          fc_horizon = 12,
                          valid_set_size = 12,
                          tsteps = 12,
                          lag_setting = 12,
                          ...) {

  `%>%` <- magrittr::`%>%`
  if (tsteps >= lag_setting) {
    max_lag <- tsteps
  }else{
    max_lag <- lag_setting
  }
  features_w_tsteps <- base::colnames(data_df)[colnames(data_df) != "key"]
  ts_name <- base::colnames(data_df)[1]
  # empty object will hold all lagged values of each input variable:
  complete_data <- NULL
  # generate lagged input variables:
  for (feature in features_w_tsteps) {
    feature_data <- dplyr::select(data_df, feature)
    train_last_pos <- (base::nrow(feature_data)
                       - valid_set_size
                       - fc_horizon)
    valid_last_pos <- (base::nrow(feature_data)
                       - fc_horizon)
    test_last_pos <- base::nrow(feature_data)
    # separate the data into different sample sets
    train_data <- feature_data[(max_lag + 1):(train_last_pos), 1] %>%
      base::as.matrix()
    valid_data <- feature_data[(train_last_pos + 1):valid_last_pos, 1] %>%
      base::as.matrix()
    test_data <- feature_data[(valid_last_pos + 1):test_last_pos, 1] %>%
      base::as.matrix()
    # add input lags to each sample set:
    lagged_train <-
      base::sapply(0:(tsteps - 1),
                   function(iter) {
                     feature_data[(iter + 1):(train_last_pos - max_lag + iter), 1]
                   }) %>%
      base::unlist() %>%
      base::matrix(., ncol = tsteps, nrow = base::nrow(train_data))
    lagged_valid <-
      base::sapply(0:(tsteps - 1),
                   function(iter) {
                     feature_data[(train_last_pos + 1 - max_lag + iter):
                                    (valid_last_pos - max_lag + iter), 1]
                   }) %>%
      base::unlist() %>%
      base::matrix(.,ncol = tsteps, nrow = base::nrow(valid_data))
    lagged_test <-
      base::sapply(0:(tsteps - 1),
                   function(iter) {
                     feature_data[(valid_last_pos + 1 - max_lag + iter):
                                    (test_last_pos - max_lag + iter), 1]
                   }) %>%
      base::unlist() %>%
      base::matrix(., ncol = tsteps, nrow = base::nrow(test_data))
    # Combine the three sample sets:
    combined_lagged_data <-
      base::rbind(
        lagged_train,
        lagged_valid,
        lagged_test) %>%
      base::as.data.frame() %>%
      {
        base::colnames(.) <- base::paste(tsteps:1, feature, sep="_")
        .
      }
    # Add the combined data of the selected variable to the rest of the lagged input variable
    complete_data <- dplyr::bind_cols(complete_data, combined_lagged_data)
    # Add the output vector to the input matrix: no lags will be added as tsteps only applies to the inputs.
    if (feature == ts_name) {
      combined_y <-
        base::rbind(train_data %>%
                      base::as.data.frame() %>%
                      dplyr::mutate(key = "Training"),
                    valid_data %>%
                      base::as.data.frame() %>%
                      dplyr::mutate(key = "Validation"),
                    test_data %>%
                      base::as.data.frame() %>%
                      dplyr::mutate(key = "Test")) %>%
        base::as.data.frame() %>%
        {
          base::colnames(.) <- c("y", "key")
          .
        }
      complete_data <- dplyr::bind_cols(complete_data, combined_y)
    }
  }
  # drop sets with size == 0:
  if (train_last_pos == valid_last_pos) {
    complete_data <-
      complete_data %>%
      dplyr::filter(key != "Validation")
  }
  if (valid_last_pos == test_last_pos) {
    complete_data <-
      complete_data %>%
      dplyr::filter(key != "Test")
  }
  return(complete_data)
}

#' Reshape regressors X
#' @param X input matrix
#' @param tsteps An integer, the number of time steps (i.e. lags) with explanatory power. These will be included as regressors.
#' @param nb_features A positive integer, number of features/regressors
reshape_X <- function(X, tsteps = 1, nb_features = 1) {
  base::dim(X) <- c(base::dim(X)[1], tsteps, nb_features)
  return(X)
}

#' Reshape target variable y
#' @param Y ouput vector
reshape_Y <- function(Y) {
  base::dim(Y) <- c(base::dim(Y)[1], 1)
  return(Y)
}

#' Apply a transformation on the data
#' @param original_data A 'ts' or 'xts' object, data to apply transformation on
#' @param transformed_ts A 'ts' or 'xts' object, data on which the transformation has been applied on
#' @param transf_method A string, method of data transformation
#' @param apply_transform boolean, if TRUE the data will be transformed, else the transformation will be undone.
transform_data <- function(original_data,
                           transformed_ts = NULL,
                           transf_method = "diff",
                           apply_transform = TRUE) {
  `%>%` <- magrittr::`%>%`
  original_data <- check_data_sv_as_xts(original_data)
  transformed_ts <- check_data_sv_as_xts(transformed_ts)
  if (!transf_method %in% c("diff", "log", "sqrt")) {
    message("The value of the 'transf_method' argument is invalid, using 'diff' as default!")
    transf_method <- "diff"
  }
  if (!base::is.logical(apply_transform)) {
    message("The value of the 'apply_transform' argument is invalid, using 'TRUE' as default!")
    apply_transform <- TRUE
  }
  if (apply_transform) {
    if (base::is.null(original_data)) {
      stop("To apply the transformation, data must be provided!")
    }
    if (transf_method == "diff") {
      transformed_ts <- base::diff(original_data)
    }
    if (transf_method == "log") {
      y <-
        (original_data
         + base::abs(base::min(base::min(original_data,
                                         na.rm = TRUE),
                               0)))
      transformed_ts <- base::log(y + 1)
    }
    if (transf_method == "sqrt") {
      y <-
        (original_data
         + base::abs(base::min(base::min(original_data,
                                         na.rm = TRUE),
                               0)))
      transformed_ts <- base::sqrt(y)
    }
    return(transformed_ts)
  } else {
    if (base::is.null(transformed_ts)) {
      stop("To undo the transformation, the transformed data must be provided!")
    }
    if (base::is.null(original_data)) {
      stop("To undo the transformation, the original untransformed data must be provided!")
    }
    if (transf_method == "diff") {
      sum_data <- original_data[1]
      theor_trans_ts <-
        base::diff(original_data) %>%
        xts::merge.xts(., transformed_ts, join = "outer") %>%
        .[, 1]
      filled_trans_ts <- theor_trans_ts
      filled_trans_ts[zoo::index(filled_trans_ts) %in% zoo::index(transformed_ts)] <- transformed_ts
      undo_theo_trans_ts <- filled_trans_ts
      for (num in 1:base::nrow(filled_trans_ts)) {
        sum_data <-
          (base::as.numeric(sum_data)
           + if (base::is.na(filled_trans_ts[num])) 0 else filled_trans_ts[num])
        undo_theo_trans_ts[num] <- sum_data
      }
      undo_trans_ts <- undo_theo_trans_ts[zoo::index(undo_theo_trans_ts) %in% zoo::index(transformed_ts)]
    }
    if (transf_method == "log") {
      undo_trans_ts <-
        (base::exp(transformed_ts)
         - 1
         - base::abs(base::min(base::min(original_data,
                                         na.rm = TRUE),
                               0)))
    }
    if (transf_method == "sqrt") {
      undo_trans_ts <-
        (transformed_ts^2
         - base::abs(base::min(base::min(original_data,
                                         na.rm = TRUE),
                               0)))
    }
    return(undo_trans_ts)
  }
}

#' Extract a univariate xts object from a mutivariate 'xts' object
#' @param mts_data_xts a univariate or multivariate 'xts' object
#' @param ind a numeric, the column index of the desired univariate 'xts' object
#' @export
univariate_xts <- function(mts_data_xts, ind = 1) {
  ts_data_xts <- NULL
  if (!xts::is.xts(mts_data_xts)) {
    stop("The data must be an xts object!")
  } else if (!base::is.numeric(ind)) {
    stop("The index number must be a numeric!")
  } else {
    ts_data_xts <-
      xts::xts(mts_data_xts[, ind],
               frequency = stats::frequency(mts_data_xts),
               order.by = zoo::index(mts_data_xts))
  }
  return(ts_data_xts)
}

#' Default preprocessing function
#' @description
#' Default preprocessing function to handle missing values in the data. This function selects the
#' largest interval of non-missing values and attributes the most recent dates to these values.
#' @param ts_data A univariate 'ts' or 'xts' object
#' @export
default_prepro_fct <- function(ts_data) {
  `%>%` <- magrittr::`%>%`
  if (!base::class(ts_data)[1] %in% c("ts", "xts")) {
    stop("The input data must be of class ts or xts!")
  }
  xts_data <- check_data_sv_as_xts(ts_data)
  contiguous_data <-
    xts_data %>%
    timeSeries::na.contiguous()
  zoo::index(contiguous_data) <-
    zoo::index(xts_data)[(base::length(xts_data)
                          - base::length(contiguous_data)
                          + 1):length(xts_data)]
  return(contiguous_data)
}
