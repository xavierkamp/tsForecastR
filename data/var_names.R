#' @param exclude_PI A boolean, to extract prediction intervals stored in 'list' object under the keywords 'upper' and 'lower'.
#' @param data_xts An 'xts' object, the original (i.e. unprocessed) time series data
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
#' @param time_id A POSIXct, timestamp created with \code{\link[base]{Sys.time}} which is then appended to the results
#' @param fc_horizon An integer, the forecasting horizon (i.e. the number of periods to forecast)
#' @param data_dir A string, directory to which results can be saved as text files
#' @param arima_arg A list, optional arguments to pass to the \code{\link[forecast]{auto.arima}} function
#' @param ets_arg A list, optional arguments to pass to the \code{\link[forecast]{ets}} function
#' @param tbats_arg A list, optional arguments to pass to the \code{\link[forecast]{tbats}} function
#' @param nnetar_arg A list, optional arguments to pass to the \code{\link[forecast]{nnetar}} function
#' @param stl_arg A list, optional arguments to pass to the \code{\link[stats]{stl}} function
#' @param snaive_arg A list, optional arguments to pass to the \code{\link[forecast]{snaive}} function
#' @param lstm_keras_arg A list, optional arguments to pass to the lstm network
#' @param bsts_arg A list, optional arguments to pass to the \code{\link[bsts]{bsts}} function
#' @param automl_h2o_arg A list, optional arguments to pass to the \code{\link[h2o]{h2o.automl}} function
#' @param mts_data A univariate or multivariate 'ts', 'mts' or 'xts' object
#' @param xreg_data A univariate or multivariate 'ts', 'mts' or 'xts' object, optional external regressors
#' @param data_colnames A vector of strings, colnames of the input data which need to be cleaned
#' @param col_ind A numeric, the column index of the desired univariate xts object
#' @param prepro_fct A function, a preprocessing function which handles missing values in the data.
#' The default preprocessing function selects the largest interval of non-missing values and then attributes the
#' most recent dates to those values. Other data handling functions can be applied (e.g. timeSeries::na.contiguous,
#' imputeTS::na.mean, custom-developed...).
#'
#' @param data_transf_method A string, the data transformation method to be passed to the function.
#' (available options: 'diff', 'log', 'sqrt')
#'
#' @param model_names A list or vector of strings representing the model names to be used
#' @param model_name A string representing the model name to be used
#' @param model_args A list, optional arguments to pass to the models
#' @param models_args A list, optional arguments to passed to the models
#' @param period_iter A string, period id (format: 'period' + '_' + iter). This id defines the iteration number of forecasting
#' exercise.
#'
#' @param ... Additional arguments to be passed to the function
#' @param trans_fct A function, function which transforms the data
#' @param tsteps An integer, the number of time steps (i.e. lags) with explanatory power. These will be included as regressors.
#' @param lag_setting An integer, the periodicity of the data. Important when dealing with seasonal data.
#' @param valid_set_size An integer, the validation set size (default = 0)
#' @param nb_features A positive integer, number of features/regressors
#' @param original_data A 'ts' or 'xts' object, data to apply transformation on
#' @param transformed_ts A 'ts' or 'xts' object, data on which the transformation has been applied on
#' @param transf_method A string, method of data transformation
#' @param apply_transform A boolean, if TRUE the data will be transformed, else the transformation will be undone.
#' @param tsfcr_obj A 'tsForecastR' object
#' @param forecast_obj A 'forecast' object
#' @param bsts_obj A bsts.prediction object
#' @param transf_fct A function, function which transforms the data
#' @param data_df A 'data.frame' object
#'
#' @param nb_cores An integer, the number of CPU cores to select
#' @param nb_threads An integer, the number of threads to use for the automl_h2o model selection process
#'
#' @param raw_data A univariate 'ts' or 'xts' object, original (i.e. unprocessed) time series data
#' @param sample_split A list, the sample split (e.g. training, validation and test sets)
#' @param input_data A 'ts', 'mts' or 'xts' object
#' @param bt_iter An integer, number of the current backtesting operation (i.e. forecasting exercise). This argument
#' must be smaller or equal to the number of backtesting operations to perform.
#'
#' @param tmp_test_set_size An integer, size of the second test set (used in the automl_h2o procedure)
#' (by default: tmp_test_set_size = 0)
#'
#' @param ts_name A string
#' @param fc_formated A data.frame
#' @param actual_formated A data.frame
#' @param split_keys A data.frame, with the sample split keys (e.g. train, valid, test)
#' @param model_descr A string,
#' @param model_par A string representing the model estimates which were previously collapsed into a single string
#' @param model_par_vec A vector of strings representing the model estimates which will be collapsed to a single string.
#' @param fc_ls A list, the forecasts must be stored under the keyword 'mean'.
#' @param default_colname a string, default colname assigned when no colnames are defined
#' @param nb_colnames an integer, number of colnames to check
#' @param ts_data A univariate 'ts' or 'xts' object
#' @param nb_features A positive integer, number of features/regressors
#' @param mts_data_xts a univariate or multivariate 'xts' object
