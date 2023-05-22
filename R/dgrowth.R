#'  Forest growth model
#' @param time time since start
#' @param C forest carbon
#' @param parms - as list with four values: r, K, g, closer
#' @param r intrinsic growth rate 
#' @param K carrying capacity (kgC)
#' @param g linear growth rate after closer
#' @param closer canopy closer (kg C)
#' @return derivative of population with time 

dgrowth <- function(time, C, parms) {
  if(C < parms$closer)
    dC <- parms$r * C
  else
    dC <- parms$g * (1 - C / parms$K)
}