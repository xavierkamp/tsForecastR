`%>%` <- magrittr::`%>%`

trend <-
  (seq(1, 144)/5)%>%
  stats::ts(start = c(1980, 1),
            frequency = 12) %>%
  base::round(digits = 0)

x <-
  (arima.sim(list(ar = c(0.8897, -0.11858),
                 ma = c(-0.2279, 0.2488)),
            n = 144,
            sd = sqrt(20)) + 100) %>%
  base::round(digits = 0) %>%
  stats::ts(start = c(1980, 1),
            frequency = 12) + trend

devtools::use_data(x)
