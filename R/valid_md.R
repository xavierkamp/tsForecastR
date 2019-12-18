valid_md_arima <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (nrow(ts_data_xts) < stats::frequency(ts_data_xts) + 1) {
    warning(paste("Not enough observations! To use 'arima', there must be more than ",
                  stats::frequency(ts_data_xts)," observations available.",
                  sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}
valid_md_ets <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (nrow(ts_data_xts) < 2) {
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
    return(NULL)
  }
}
valid_md_stl <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (stats::frequency(ts_data_xts) <= 1) {
    warning("The data is not a seasonal object! To use 'stl', the data frequency must be higher than 1.
            Otherwise, the seasonal component cannot be estimated.")
    return(FALSE)
  } else if (nrow(ts_data_xts) < 2*stats::frequency(ts_data_xts) + 1) {
    warning(paste("Not enough observations! To use 'stl', there must be more than ",
                  stats::frequency(ts_data_xts)," observations available.",
                  sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}
valid_md_tbats <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (nrow(ts_data_xts) < 2) {
    warning("Not enough observations! To use 'tbats', there must be more than 2 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
valid_md_nnetar <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (nrow(ts_data_xts) < 3) {
    warning("Not enough observations! To use 'nnetar', there must be more than 3 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
valid_md_bsts <- function(ts_data_xts) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  if (nrow(ts_data_xts) < 3) {
    warning("Not enough observations! To use 'bsts', there must be more than 3 observations available.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
valid_md_lstm_keras <- function(ts_data_xts, lstm_keras_arg) {
  ts_data_xts <- check_data_sv_as_xts(ts_data_xts)
  min_nb_obs <-
    (max(lstm_keras_arg$lag_setting, lstm_keras_arg$nb_timesteps)
     + lstm_keras_arg$valid_set_size
     + 1)
  if (nrow(ts_data_xts) < min_nb_obs) {
    warning(paste("Not enough observations! To use 'lstm_keras', there must be more than ",
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
     + automl_h2o_arg$tmp_test_set_size)
  if (nrow(ts_data_xts) < min_nb_obs) {
    warning(paste("Not enough observations! To use 'automl_h2o', there must be more than ",
                  min_nb_obs, " observations available.",
                  sep = ""))
    return(FALSE)
  } else {
    return(TRUE)
  }
}
