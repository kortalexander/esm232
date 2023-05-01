#' Compute profit from almonds using baseline yield and yield anomalies
#' 
#' @param yield_anomalies list of annual yield anomalies. Tons/acre
#' @param baseline_yield baseline yield of almonds that the anomalies deviate from. Tons/acre.
#' @param price_per_ton sell price of almonds. $US/ton
#' @param acres acreage of almond farm
#' @return profit of almond yield. $US

compute_profit <- function(yield_anomalies, baseline_yield = 1, price_per_ton = 3000, acres = 100) {
  profit <- (baseline_yield + yield_anomalies) * acres * price_per_ton
  return(list(mean_profit = mean(profit)))
}