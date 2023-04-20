#' Calculate the energy produced by a solar panel given the annual radiation
#' 
#' @param area Area of the solar panel(s) in m^2.
#' @param panel_yield Efficiency of the panel in converting solar radiation to energy. 0-1, default is 0.2.
#' @param annual_radiation Average annual solar radiation in kWh landing on the panel(s).
#' @param performance_ratio Other factors that impact efficiency. 0-1, default is 0.75.
#' @returns Energy produced by the solar panel over a year in kWh.
#' @examples
#' solar_energy(area = 2, annual_radiation = 1000)
#' solar_energy(area = 4, panel_yield = 0.5, annual_radiation = 500, performance_ratio = 0.8)
#' 

solar_energy <- function(area, panel_yield = 0.2, annual_radiation, performance_ratio = 0.75){
  energy_produced <- area * panel_yield * annual_radiation * performance_ratio
  return(energy_produced)
}