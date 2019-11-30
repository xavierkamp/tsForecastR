# Preprocessing functions:
# This file contains functions used in the preprocessing steps in the forecasting procedures.

#' Add empty placeholders
#'
#' @param input_data
#' @return
add_placeholders <- function(input_data, fc_horizon, backtesting_opt = NULL) {
  `%>%` <- magrittr::`%>%`
  input_data_xts <- check_data_sv_as_xts(input_data)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  if (backtesting_opt$use_backtesting) {
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
#'
#' @param input_data
#' @return
add_features <- function(input_data, xreg_data = NULL) {
  `%>%` <- magrittr::`%>%`
  input_data_xts <- check_data_sv_as_xts(input_data)
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
#'
#' @param input_data
#' @return
split_train_test_set <- function(input_data, fc_horizon = 12, bt_iter = 1,
                                 valid_set_size = 0, tmp_test_set_size = 0,
                                 backtesting_opt = NULL,
                                 ...) {
  `%>%` <- magrittr::`%>%`
  input_data_xts <- check_data_sv_as_xts(input_data)
  fc_horizon <- check_fc_horizon(fc_horizon)
  valid_set_size <- check_valid_set_size(valid_set_size)
  tmp_test_set_size <- check_tmp_test_set_size(tmp_test_set_size)
  backtesting_opt <- check_backtesting_opt(backtesting_opt)
  bt_iter <- check_backtesting_iter(bt_iter, backtesting_opt)
  backtesting_set_size <- backtesting_opt$backtesting_set_size
  backtesting_method <- backtesting_opt$backtesting_method
  nb_periods <- backtesting_opt$backtesting_nb_iters
  if (backtesting_opt$use_backtesting) {
    if ((backtesting_set_size[1] == 'expanding') & (backtesting_method[1] == 'rolling')) {
      last_train_pos <-
        (base::nrow(input_data_xts)
         - fc_horizon
         - nb_periods
         + bt_iter
         - valid_set_size
         - tmp_test_set_size)
      x_train <- input_data_xts[1:(last_train_pos), ]
    } else if ((backtesting_set_size[1] == 'expanding' & backtesting_method[1] == 'moving')) {
      last_train_pos <-
        (base::nrow(input_data_xts)
         - fc_horizon * (nb_periods - bt_iter + 1)
         - valid_set_size
         - tmp_test_set_size)
      x_train <- input_data_xts[1:(last_train_pos), ]
    } else if ((backtesting_set_size[1] == 'fixed' & backtesting_method[1] == 'rolling')) {
      last_train_pos <-
        (base::nrow(input_data_xts)
         - fc_horizon
         - nb_periods
         + bt_iter
         - valid_set_size
         - tmp_test_set_size)
      x_train <- input_data_xts[(1 + bt_iter - 1):(last_train_pos), ]
    } else if ((backtesting_set_size[1] == 'fixed' & backtesting_method[1] == 'moving')) {
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
  split <- list(x_train, x_valid, x_tmp_test, x_test)
  base::names(split) <- c("train", "valid", "tmp_test", "test")
  return(split)
}

#' Normalize the data
#'
#' @param data_df
#' @return
normalize_data <- function(data_df) {
  `%>%` <- magrittr::`%>%`
  if (!base::is.data.frame(data_df)) {
    stop("The data must be a data.frame obj before normalizing!")
  }
  df <- data_df
  features_to_norm <- base::colnames(df)[base::colnames(df) != "key"]
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
  }
  return(df)
}

#' Add timesteps
#' Description:
#' for lstm, when the number of time steps (i.e. number of lags) is defined,
#' for each input variable, the required number of lagged values needs to be
#' added as inputs to the input matrix
#'
#' when the ts is seasonal with periodicity = 12, then lag_setting should be 12.
#' The volume of the same month of previous year will most likely have a high
#' influence on today's volume.
#' when the ts is non-seasonal, lagSetting = 1, i.e. the volume of previous
#' month will most likely have the strongest impact on today.
#'
#' when lag_setting = 12 and tsteps < lagSetting (e.g. tsteps=4),
#' then input values will be: t-12, t-11, t-10, t-9
#' when lag_setting = 12 and tsteps = lagSetting, then input values will be:
#' t-12, t-11, ... , t-1
#' when lagSetting = 12 and tsteps > lagSetting (e.g. tsteps = 15),
#' then input values will be: t-15, ... , t-12, ... , t-1
#'
#' Same goes for lagSetting other than 12.
#'
#' @param data_df
#' @return
add_timesteps <- function(data_df, fc_horizon = 12,
                          valid_set_size = 12,
                          tsteps = 12, lag_setting = 12,
                          backtesting_opt = list(use_backtesting = FALSE,
                                                 backtesting_nb_iter = 1,
                                                 backtesting_method = c("rolling",
                                                                        "moving"),
                                                 backtesting_set_size = c("expanding",
                                                                          "fixed"))) {
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

#' Check validation set size
#' This function ensures that the selected validation set size is valid.
#' If not, the function throws an error.
#'
#' @param valid_set_size must be a positive integer
#' @return valid_set_size: a positive integer
reshape_X_3d <- function(X, tsteps = 1, nb_features = 1) {
  base::dim(X) <- c(base::dim(X)[1], tsteps, nb_features)
  return(X)
}

#' Check validation set size
#' This function ensures that the selected validation set size is valid.
#' If not, the function throws an error.
#'
#' @param valid_set_size must be a positive integer
#' @return valid_set_size: a positive integer
reshape_Y_2d <- function(X) {
  base::dim(X) <- c(base::dim(X)[1], 1)
  return(X)
}
