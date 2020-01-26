if (require(testthat)) {
  test_that("read_tsForecastR_works", {
    df <- read_tsForecastR(NULL)
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_arima(ts_data)
    df <- read_tsForecastR(fc)
    expect_equal(nrow(df), 156)
    model_names <- c("arima", "ets", "stl")
    fc <- generate_fc(ts_data, model_names = model_names)
    df <- read_tsForecastR(fc)
    expect_equal(nrow(df), 156 * 3)
  })
  test_that("read_fc_from_file_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    # create a tmp directory where results will be saved
    save_fc_to_file <- base::paste(getwd(), "tmp_testing_dir", sep = "/")
    while (dir.exists(save_fc_to_file)) {
      save_fc_to_file <- base::paste(save_fc_to_file, "1", sep = "")
    }
    model_names <- check_model_names(NULL)
    dir.create(save_fc_to_file)
    # generate forecasts and save in files
    fc <- generate_fc(ts_data,
                      model_names = model_names,
                      save_fc_to_file = save_fc_to_file)
    # check if files are properly named
    for (ts_name in base::names(fc)) {
      for (i in model_names) {
        file_name <- base::paste(save_fc_to_file,
                                 base::paste(ts_name,
                                             i,
                                             sep = "_"),
                                 sep = "/")
        expect_equal(base::file.exists(file_name), TRUE)
      }
    }
    expect_error(generate_fc(ts_data, model_names = model_names,
                             save_fc_to_file = "1234"))
    # read files
    df <-
      read_fc_from_file(base::colnames(ts_data),
                        save_fc_to_file = save_fc_to_file,
                        model_names = model_names)
    expect_equal(base::nrow(df), base::length(model_names)*156)
    # delete tmp dir with files
    base::unlink(save_fc_to_file, recursive = TRUE)
  })
  test_that("save_as_df_works", {
    data <- seq(1:144)
    ts_data <- stats::ts(data, frequency = 12, start = c(1, 1))
    fc <- generate_fc_arima(ts_data)
    df <- save_as_df(fc)
    expect_equal(is.data.frame(df), TRUE)
    # create tmp directory where results will be saved
    save_fc_to_file <- base::paste(getwd(), "tmp_testing_dir", sep = "/")
    while (base::dir.exists(save_fc_to_file)) {
      save_fc_to_file <- base::paste(save_fc_to_file, "1", sep = "")
    }
    base::dir.create(save_fc_to_file)
    # generate forecasts and save in files
    fc <- generate_fc_arima(ts_data,
                            save_fc_to_file = save_fc_to_file)
    # read files
    df <-
      save_as_df(data_colnames = "time_series_1",
                 save_fc_to_file = save_fc_to_file,
                 model_names = "arima")
    expect_equal(base::nrow(df), 156)
    expect_equal(base::ncol(df), 15)
    expect_equal(base::is.data.frame(df), TRUE)
    # delete tmp dir with files
    base::unlink(save_fc_to_file, recursive = TRUE)
  })
}
