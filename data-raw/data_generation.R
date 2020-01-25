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



get_event_pos <- function(nb_events,
                          length_data,
                          event_cooldown = 3,
                          grid_search_perc = 0.15,
                          Plot = FALSE) {
  `%>%` <- magrittr::`%>%`
  break_pos <- data.frame()
  if (nb_events <= 0) {
    return(break_pos)
  } else {
    radial_num <- pi * seq(0:length_data)/length_data
    density_break <- rep(0, length_data + 1)
    for (i in 1:nb_events) {
      amplitude <- 1 + 2*(i - 1)
      density_break <- density_break + sin(amplitude * radial_num)/amplitude
    }
    proba_break <- density_break / sum(density_break)
    rgrid_search <-
      sample(seq(1:nb_obs),
             round(nb_obs * grid_search_perc),
             replace = FALSE)
    break_pos <-
      proba_break[rgrid_search] %>%
      as.data.frame() %>%
      {
        base::colnames(.) <- "proba"
        .
      } %>%
      cbind(rgrid_search) %>%
      dplyr::arrange(desc(proba)) %>%
      dplyr::distinct() %>%
      {
        base::colnames(.) <- c("proba", "event_pos")
        .
      }
    if (nrow(break_pos) > 1) {
      filtered_break_pos <- break_pos
      i <- 1
      while (i < nrow(filtered_break_pos)) {
        min_pos <- filtered_break_pos[i, "event_pos"]
        filtered_break_pos <-
          filtered_break_pos %>%
          dplyr::mutate(dist = abs(min_pos - event_pos)) %>%
          dplyr::filter(dist > event_cooldown | dist == 0) %>%
          dplyr::select(proba, event_pos)
        i <- i + 1
      }
    } else {
      filtered_break_pos <- break_pos
    }
    if (nrow(filtered_break_pos) < nb_events) {
      warning(paste("The number of available event periods is lower than the number of events selected. ",
                    "In this case, all available periods will be selected. To avoid this warning, ",
                    "either decrease the event_cooldown, decrease the number ",
                    "of events or increase the length of the data.", sep = ""))
    } else {
      filtered_break_pos <-
        filtered_break_pos %>%
        head(nb_events)
    }
    if (Plot) {
      plot(proba_break,
           main = "Density function of event occurence across all possible time periods",
           xlab = "time period",
           ylab = "proba of event",
           type = "l")
      legend("bottom", "selected events", fill = "blue")
      points(x = filtered_break_pos$event_pos,
             y = filtered_break_pos$proba, col = "blue", pch = 19)
    }
    return(filtered_break_pos)
  }
}

distorted_normal <- function(var_mean, var_sd, var_skew, var_kurtosis) {

}

# trend effect
trend_pos <- get_event_pos(1, 100)
event_ampl <- trend_ampl
event_pos <- trend_pos
if (event_effects == "random") {
  runif(nrow(event_pos), min = event_ampl$min, max = event_ampl$max)

}




sim_ts_data <- function(nb_obs,
                        data_freq = 12,
                        only_pos_val = FALSE,
                        use_rounding = FALSE,
                        ini_value = 100,
                        event_cooldown = 3,
                        nb_trends = 0,
                        trend_effects = c("random", c(0.1, 0.2, 0.3)),
                        trend_ampl = list(min = 0.3, max = 0.4),
                        trend_signs = c("all", "positive", "negative"),
                        nb_str_breaks = 0,
                        break_effects = c("random", c(0.3, 0.4, 0.2)),
                        break_ampl = list(min = 0.3, max = 0.4),
                        break_signs = c("all", "positive", "negative"),
                        seasonal = TRUE,
                        const_seasonal = TRUE,
                        seasonal_effects = c("random", c(0.3, 0.1, 0.4)),
                        seasonal_ampl = list(min = 0.3, max = 0.5),
                        seasonal_skew = 0,
                        seasonal_kurt = 0,
                        seasonal_growth = c("increasing", "decreasing"),
                        noise_seasonal_ratio = 0.1,
                        noise_ampl = 0.4,
                        noise_lags = 0,
                        seed = NULL,
                        ...) {
  set.seed(seed)


  if (constr_trend) {
    trend_effects <- c(runif(min = 0.2, max = 0.4))

  } else {

  }

  #### Select: Structural Breaks ####
  ## Nb of breaks
  nb_str_breaks <- 1
  ## Proba of breaks
  if (nb_str_breaks > 0) {
    radial_num <- pi * seq(0:nb_obs)/nb_obs
    density_break <- rep(0, nb_obs)
    for (i in 1:nb_str_breaks) {
      amplitude <- 1 + 2*(i - 1)
      density_break <- density_break + sin(amplitude * radial_num)/amplitude
    }
    proba_break <- density_break / sum(density_break)
    plot(proba_break)
    rgrid_search <-
      runif(n = round(nb_obs * 0.10), min = 1, max = nb_obs) %>%
      round(.)
    break_pos <-
      proba_break[rgrid_search] %>%
      as.data.frame() %>%
      {
        base::colnames(.) <- "proba"
        .
      } %>%
      cbind(rgrid_search) %>%
      dplyr::arrange(proba) %>%
      tail(nb_str_breaks)
    print(break_pos)
    break_effect_ampl <- runif(nb_str_breaks, min = 0.2, max = 0.7)
    break_effect_sign <- sample(c(-1, 1), nb_str_breaks, replace = TRUE)
    break_effect <- break_effect_ampl * break_effect_sign
    break_info <-
      break_pos %>%
      cbind(break_effect)
  }


}


get_event_effect <- function(df, var_ampl){

}
  break_effect_ampl <- runif(nb_str_breaks, min = 0.2, max = 0.7)
  break_effect_sign <- sample(c(-1, 1), nb_str_breaks, replace = TRUE)
  break_effect <- break_effect_ampl * break_effect_sign
  break_info <-
    break_pos %>%
    cbind(break_effect)




#### Select: Length of ts ####
nb_obs <- 100

#### Select: Data frequency ####
data_freq <- 1

#### Select: Positive or All ####
only_pos_val <- TRUE

#### Select: Round or None ####
use_rounding <- FALSE

#### Select: Initial value ####
x_0 <- rnorm(1, mean = 100)

#### Select: Trend ####
## n = 0 (None)
nb_trends <- 0
## n = 1
nb_trends <- 1
## n > 1 (Max 3)
nb_trends <- min(2, 3)
## Fixed or variable
const_trend <- TRUE

if (constr_trend) {
  trend_effects <- c(runif(min = 0.2, max = 0.4))

} else {

}

#### Select: Structural Breaks ####
## Nb of breaks
nb_str_breaks <- 1
## Proba of breaks
if (nb_str_breaks > 0) {
  radial_num <- pi * seq(0:nb_obs)/nb_obs
  density_break <- rep(0, nb_obs)
  for (i in 1:nb_str_breaks) {
    amplitude <- 1 + 2*(i - 1)
    density_break <- density_break + sin(amplitude * radial_num)/amplitude
  }
  proba_break <- density_break / sum(density_break)
  plot(proba_break)
  rgrid_search <-
    runif(n = round(nb_obs * 0.10), min = 1, max = nb_obs) %>%
    round(.)
  break_pos <-
    proba_break[rgrid_search] %>%
    as.data.frame() %>%
    {
      base::colnames(.) <- "proba"
      .
    } %>%
    cbind(rgrid_search) %>%
    dplyr::arrange(proba) %>%
    tail(nb_str_breaks)
  print(break_pos)
  break_effect_ampl <- runif(nb_str_breaks, min = 0.2, max = 0.7)
  break_effect_sign <- sample(c(-1, 1), nb_str_breaks, replace = TRUE)
  break_effect <- break_effect_ampl * break_effect_sign
  break_info <-
    break_pos %>%
    cbind(break_effect)
}

#### Select: Seasonality ####
## n = 0 (None)

## n = fixed
## n = variable
#### Select: Disturbance ####

#### Select: Autocorrelation ####
#### Select: Missing values ####
