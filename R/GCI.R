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
#' log-periodogram.
#'
#' @param x, dataframe with raw CGM time series or object of class "6DF_summary"
#' containing 6-DF representation of log-periodogram
#' @param weights, numeric vector of weights used to form the GCI linear combination
#' @param means, numeric vector of mean values for the 6 degree of freedom
#' approximation, subtracted away before calculation of GCI
#' @param stddevs, numeric vector of standard deviations for each of the values
#' within the 6 degree of freedom approximation, used for scaling before GCI calculation
#' @param ..., additional arguments/settings required
#'
#' @return gci, numeric containing the Glucose Color Index variability metric value
#'
#' @export
#'
calculate_GCI <- function(x, weights, means, stddevs, ...) UseMethod("calculate_GCI")


#' Calculates Glucose Color Index (GCI) using 6DF disjoint linear summary of
#' log-periodogram
#'
#' @details Using weights, defaulting to those derived from analysis of the repeat
#' CGM wears in the HYPNOS trial, generates the GCI as a weighted linear combination
#' of a provided 6 degree of freedom disjoint piece-wise linear approximation of
#' the log-periodogram.
#'
#' @param x, Object of class "6DF_summary" containing 6-DF representation of
#' log-periodogram
#' @param weights, numeric vector of weights used to form the GCI linear combination
#' @param means, numeric vector of mean values for the 6 degree of freedom
#' approximation, subtracted away before calculation of GCI
#' @param stddevs, numeric vector of standard deviations for each of the values
#' within the 6 degree of freedom approximation, used for scaling before GCI calculation
#' @param ..., additional arguments/settings (not required here)
#'
#' @return gci, numeric containing the Glucose Color Index variability metric value
#'
#' @export
#'
calculate_GCI.6DF_summary <- function(x, weights = default_weights(),
                          means = c(long_slope = 0, long_midpoint = 0,
                                    int_slope = 0, int_midpoint = 0,
                                    short_slope = 0, short_midpoint = 0),
                          stddevs = c(long_slope = 1, long_midpoint = 1,
                                      int_slope = 1, int_midpoint = 1,
                                      short_slope = 1, short_midpoint = 1),
                          ...){
  reqnames = c("long_slope", "long_midpoint", "int_slope", "int_midpoint",
               "short_slope", "short_midpoint")

  if(!all(reqnames %in% names(means)) | !all(reqnames %in% names(stddevs))){
    stop("Insufficient specification for standardization")
  }

  result = 0
  for(dfmet in names(x)){
    result = result + (x[[dfmet]] - means[dfmet])/stddevs[dfmet] * weights[dfmet]
  }
  return(unname(result))
}

#' Calculates Glucose Color Index (GCI) using long-format raw CGM time series
#' in a dataframe with sensible defaults
#'
#' @details Using weights, defaulting to those derived from analysis of the repeat
#' CGM wears in the HYPNOS trial, generates the GCI as a weighted linear combination
#' of a provided 6 degree of freedom disjoint piece-wise linear approximation of
#' the log-periodogram. If columnnames are not properly specified, an error will
#' be thrown.
#'
#' @param x, dataframe containing time argument in "timestamp" column
#' and CGM argument in "glucose" column
#' @param weights, numeric vector of weights used to form the GCI linear combination
#' @param means, numeric vector of mean values for the 6 degree of freedom
#' approximation, subtracted away before calculation of GCI
#' @param stddevs, numeric vector of standard deviations for each of the values
#' within the 6 degree of freedom approximation, used for scaling before GCI calculation
#' @param ..., can include list of periodogram options: number of expected days
#' in the data ("days"), maximum gap in the data allowable ("maxgap", in days),
#' minimum data for analysis ("mindata" in days), amount of start and end data to
#' trim ("strim" and "etrim" both in days), and odd-value spans of Daniel smoothing
#' kernels ("spans"). This argument is required, and a set of reasonable
#' defaults is provided by the default_pgram() function.
#'
#' @return gci, numeric containing the Glucose Color Index variability metric value
#'
#' @export
#'
calculate_GCI.data.frame <- function(x, weights = default_weights(),
                                      means = c(long_slope = 0, long_midpoint = 0,
                                                int_slope = 0, int_midpoint = 0,
                                                short_slope = 0, short_midpoint = 0),
                                      stddevs = c(long_slope = 1, long_midpoint = 1,
                                                  int_slope = 1, int_midpoint = 1,
                                                  short_slope = 1, short_midpoint = 1), ...){
  reqnames = c("long_slope", "long_midpoint", "int_slope", "int_midpoint",
               "short_slope", "short_midpoint")

  if(!all(reqnames %in% names(means)) | !all(reqnames %in% names(stddevs))){
    stop("Insufficient specification for standardization")
  }

 if(length(list(...))>0){
   est_lpgram = pgram(x$glucose, x$timestamp, ...)
 }
 else{
   est_lpgram = pgram(x$glucose, x$timestamp)
 }
 summary_6DF = linear_approx(est_lpgram)
 calculate_GCI(summary_6DF, weights, means, stddevs)
}

#' Catch attempt to calculate GCI using unsupported class
#'
#' @param x, Object of class other than "6DF_summary" or dataframe
#' @param weights, numeric vector of weights used to form the GCI linear combination
#' @param means, numeric vector of mean values for the 6 degree of freedom
#' approximation, subtracted away before calculation of GCI
#' @param stddevs, numeric vector of standard deviations for each of the values
#' within the 6 degree of freedom approximation, used for scaling before GCI calculation
#' @param ..., additional arguments/settings (not required here)
#'
#' @export
#'
calculate_GCI.default <- function(x, weights, means, stddevs, ...){
  stop("GCI calculation not yet supported for this object type")
}
