#' Return default weights for 6DF representation linear combination used to
#' calculte GCI
#'
#' @details Weights derived using HYPNOS data, derived in paper introducing the
#' GCI (citation)
#'
#' @return weights, vector containing the weights derived from the HYPNOS repeat
#' CGM wears
#'
default_weights <- function(){
  return(hypnos_weights)
}

#' Calculates Glucose Color Index (GCI)
#'
#' @details Using weights, defaulting to those derived from analysis of the repeat
#' CGM wears in the HYPNOS trial, generates the GCI as a weighted linear combination
#' of the 6 degree of freedom disjoint piece-wise linear approximation of the
#' log-periodogram. All vector arguments should be named using the standard
#' convention for these metrics, which is period_metric for periods of long, int,
#' and short, metrics of slope and intercept.
#'
#' @param linear_approx, numeric vector containing 6-DF representation of
#' log-periodogram
#' @param weights, numeric vector of weights used to form the GCI linear combination
#' @param means, numeric vector of mean values for the 6 degree of freedom
#' approximation, subtracted away before calculation of GCI
#' @param stddevs, numeric vector of standard deviations for each of the values
#' within the 6 degree of freedom approximation, used for scaling before GCI calculation
#'
#' @return gci, numeric containing the Glucose Color Index variability metric value
#'
#' @export
#'
calculate_GCI <- function(linear_approx, weights = default_weights(),
                          means = c(long_slope = 0, long_midpoint = 0,
                                    int_slope = 0, int_midpoint = 0,
                                    short_slope = 0, short_midpoint = 0),
                          stddevs = c(long_slope = 1, long_midpoint = 1,
                                      int_slope = 1, int_midpoint = 1,
                                      short_slope = 1, short_midpoint = 1)){
  reqnames = c("long_slope", "long_midpoint", "int_slope", "int_midpoint",
               "short_slope", "short_midpoint")

  if(!all(reqnames %in% names(means)) | !all(reqnames %in% names(stddevs))){
    stop("Insufficient specification for standardization")
  }

  result = 0
  for(dfmet in names(linear_approx)){
    result = result + (linear_approx[dfmet] - means[dfmet])/stddevs[dfmet] * weights[dfmet]
  }
  return(unname(result))
}
