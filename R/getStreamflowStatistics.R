#' Get streamflow statistics
#'
#' @param timeseries_dir Character; path to time series folder of CAMLES-DE
#' 
#' @return A data frame containing CAMLES-DE time series streamflow data
#'
#' @examples
#'
#' @export

#library(dplyr)
#timeseries_camels_combine <- "C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization/data/CAMELS_DE_hydromet_timeseries_combine.csv"
#variable_name <- c("discharge_spec_obs", "precipitation_mean")
#start_date <- as.Date("2001-01-01")
#end_date <- as.Date("2020-12-31")
#max_missing <- 5
#data.table::fwrite(streamflow_statistics, 
#                   "C:/Users/nguyenta/Documents/LEILA/working_code_documentation/data/hydrological_indicators.csv")

# Show a popup at the given location
getStreamflowStatistics <- function(timeseries_camels_combine,
                                    variable_name,
                                    start_date, 
                                    end_date,
                                    max_missing) {
  
  message("Reading streamflow data...")
  streamflow_precipitation <- data.table::fread(timeseries_camels_combine) %>%
    tibble::as_tibble()
  
  # Get selected period and variable name
  streamflow_precipitation <- streamflow_precipitation %>% 
    select(all_of(c("date", "gauge_id", {{variable_name}}))) %>% 
    filter(date >= start_date, 
           date <= end_date) 
  
  message("Removing stations with missing data more than certain %...")
  ndays <- as.numeric(end_date - start_date) + 1
  nyears <- ndays/365.24
  
  missing <- streamflow_precipitation %>%
    group_by(gauge_id) %>%
    summarise(missing_percentage = 
                100*sum(is.na(discharge_spec_obs))/ndays) %>%
    filter(missing_percentage > max_missing)
  
  streamflow_precipitation <- streamflow_precipitation %>%
    filter(!gauge_id %in% missing$gauge_id)
  
  # Add date, month, season into the data frame
  streamflow_precipitation <- streamflow_precipitation %>%
    mutate(month = lubridate::month(date),
           year = lubridate::year(date),
           season = case_when(
             month %in% c(12, 1, 2) ~ "winter",
             month %in% c(3, 4, 5)  ~ "spring",
             month %in% c(6, 7, 8)  ~ "summer",
             month %in% c(9, 10, 11) ~ "autumn"
           )) 
  
  
  message("Calculating daily streamflow statistics...")
  daily_q_statistics <- streamflow_precipitation %>%
    group_by(gauge_id) %>%
    summarise(q_mean = mean(discharge_spec_obs, na.rm = TRUE),
              q_std = sd(discharge_spec_obs, na.rm = TRUE),
              q_5 = quantile(discharge_spec_obs, 0.05, na.rm= TRUE),
              q_95 = quantile(discharge_spec_obs, 0.95, na.rm= TRUE),
              runoff_coefficient = sum(discharge_spec_obs, na.rm = TRUE)/
                sum(precipitation_mean, na.rm = TRUE),
              p_mean = mean(precipitation_mean, na.rm = TRUE),
              p_std = sd(precipitation_mean, na.rm = TRUE),
              .groups = "drop")
  
  message("Calculatin CVQ by season...")
  cv_q <- streamflow_precipitation %>%
    group_by(gauge_id, season) %>%
    summarise(cv_q = sd(discharge_spec_obs, na.rm = TRUE)/mean(
      discharge_spec_obs, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = season, values_from = cv_q, names_prefix = "cvq_")
  
  message("Calculating minimum 7day average flow in 10 years (Q7,10)...")   
  q_7_10 <- streamflow_precipitation %>%
    group_by(gauge_id) %>%
    mutate(q_7 = data.table::frollmean(
      discharge_spec_obs, n = 7, na.rm = FALSE)) %>%
    group_by(gauge_id, year) %>%
    summarise(q_7_10 = min(q_7, na.rm = TRUE),
              .groups = "drop")
  
  q_7_10$q_7_10[which(is.infinite(q_7_10$q_7_10))] <- NA
  
  q_7_10 <- q_7_10 %>% 
    group_by(gauge_id) %>% 
    summarise(q_7_10 = quantile(q_7_10, 10/nyears, na.rm = TRUE))
  
  message("Calculating seasonal amplitude...")
  q_seasonal_amplitude <- streamflow_precipitation %>%
    group_by(gauge_id, month) %>%
    summarise(q_monthly_mean = mean(discharge_spec_obs, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(gauge_id) %>%
    summarise(q_seasonal_amplitude = max(q_monthly_mean, na.rm = TRUE) -
                min(q_monthly_mean, na.rm = TRUE),
              .groups = "drop")
  
  message("Calculating trend in streamflow...")
  q_trend <- streamflow_precipitation %>%
    group_by(gauge_id, year) %>%
    summarise(q_annual_mean = mean(discharge_spec_obs, na.rm = TRUE),
              p_annual_mean = mean(precipitation_mean, na.rm = TRUE),
              q_annual_mean_normal = q_annual_mean/p_annual_mean,
              .groups = "drop") %>% 
    group_by(gauge_id) %>%
    summarise(q_annual_sens_slope = sens.slope(na.omit(q_annual_mean_normal))$estimates,
              q_annual_p.value = sens.slope(na.omit(q_annual_mean_normal))$p.value)
  
  streamflow_statistics <- daily_q_statistics %>%
    left_join(q_7_10, by = "gauge_id")  %>%
    left_join(q_seasonal_amplitude, by = "gauge_id") %>%
    left_join(cv_q, by = "gauge_id") %>%
    left_join(q_trend, by = "gauge_id")
  
  message("Done")
  return(streamflow_statistics)
}

