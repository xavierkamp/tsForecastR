#' Analyze forecasts
#' @param fc A list
#' @param ts_name A string
#' @param period A numeric
#' @param model_names A vector of strings
#' @param hybrid_models A list
#' @param error_measure A vector of strings
#' @param Plot A boolean
#' @return A data frame
#' @export
analyze_fc <- function(fc,
                       ts_name = NULL,
                       period = NULL,
                       model_names = NULL,
                       hybrid_models = NULL,
                       error_measure = c("mse, mae, mape"),
                       Plot = TRUE) {
  `%>%` <- magrittr::`%>%`
  results <- NULL
  for (ts_name in base::names(fc)) {
    for (model in base::paste("fc$", ts_name, sep = "") %>%
         base::parse(text = .) %>%
         base::eval() %>%
         base::names()) {
      for (periods in base::paste("fc$", ts_name, "$", model, sep = "") %>%
           base::parse(text = .) %>%
           base::eval() %>%
           base::names()) {
        results <- dplyr::bind_rows(results, fc[[ts_name]][[model]][[periods]])
      }
    }
  }
  if (Plot) {
    p <-
      plotly::plot_ly(data = results,
                      x = ~dates,
                      y = ~values,
                      color = ~model) %>%
      plotly::add_lines()
    print(p)
  }
}
