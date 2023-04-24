#' Calculate the maximum, minimum, and mean almond yield anomaly for a given time series.
#' 
#' @param clim_data Data frame of daily climate data for time period of interest. Columns: day, month, year, wy, tmax_c, tmin_c, precip.
#' @returns List of max, min, and mean almond yield anomalies.
#' 

almond_yield <- function(clim_data) {
  
  # process climate data into monthly averages
  clim_monthly <- clim_data %>% 
    group_by(year, month) %>% 
    summarize(tmax = mean(tmax_c), tmin = min(tmin_c), total_precip = sum(precip))
  
  # get years of interest and loop through them
  years = unique(clim_monthly$year)
  yield_anomalies <- list()
  for(i in min(years):max(years)) {
    
    # filter data for each year
    year_i_data <- clim_monthly %>% 
      filter(year == i)
    
    # skip year if there's no data for the months needed
    if(!(1 %in% year_i_data$month) | !(2 %in% year_i_data$month)) {
      next
    }
    
    # extract needed values by referencing the row and column and input into formula
    yield_anomaly_i <- -0.015 * (year_i_data[year_i_data$month==2, ][[4]]) -
      0.0046 * (year_i_data[year_i_data$month==2, ][[4]])^2 -
      0.07 * (year_i_data[year_i_data$month==1, ][[5]]) +
      0.0043 * (year_i_data[year_i_data$month==1, ][[5]])^2 +
      0.28
    
    # add to list of anomalies
    yield_anomalies <- yield_anomalies %>% append(yield_anomaly_i)
  }
  
  # unlist anomalies and return max, min, and mean
  yield_anomalies <- unlist(yield_anomalies)
  return(list(max_yield_anomaly = max(yield_anomalies), min_yield_anomaly = min(yield_anomalies), mean_yield_anomaly = mean(yield_anomalies)))
}