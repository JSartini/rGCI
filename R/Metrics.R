#' Fit disjoint linear model portion and return slope/midpoint components
#'
#' @details Helper function to fit a linear model then extract the slope and
#' midpoint parameters
#'
#' @param subset_df, dataframe with subset of periodogram data corresponding to
#' a particular frequency band
#' @param freq_mid, frequency argument at which to evaluate the model to estimate
#' the periodogram midpoint over the given frequency band
#'
#' @return param_list, named list of summary parameters for the linear model over
#' the frequency band of interest
#'
slope_midpoint <- function(subset_df, freq_mid){
  sub_fit = lm(spectra ~ frequencies, data = subset_df)
  s = sub_fit$coefficients[2]
  m = predict(sub_fit, newdata = data.frame(frequencies = c(freq_mid)))
  param_list = list(Slope = unname(s), Midpoint = unname(m))
  return(param_list)
}

#' Calculate 6 degree of freedom representation of a log-periodogram
#'
#' @details Fits disjoint piece-wise linear models over 3 periodicity bands of
#' interest (greater than a day, a day to 150 minutes, shorter than 150 minutes),
#' returns the slopes and midpoints to form a 6-df representation of the full
#' periodogram
#'
#' @param pgram, numeric vector of log-periodogram values
#' @param freqs, numeric vector of frequencies (min^-1) corresponding to
#' the periodogram values
#'
#' @return pgram_summary, named vector containing the desired 6 degree of freedom
#' representation
#'
#' @export
#'
linear_approx <- function(pgram, freqs){
  fit_df = data.frame(spectra = pgram, frequencies = freqs, periods = 1/freqs)

  long_idxs = which(fit_df$periods >= 1440)
  long_df = fit_df[long_idxs,]
  long_period_params = slope_midpoint(long_df, mean(long_df$frequencies))

  short_idxs = which(fit_df$periods < 150)
  short_df = fit_df[short_idxs,]
  short_period_params = slope_midpoint(short_df, mean(short_df$frequencies))

  int_idxs = c(long_idxs[length(long_idxs)],
               which(fit_df$periods < 1440 & fit_df$periods >= 150),
               short_idxs[1])
  int_df = fit_df[int_idxs,]
  int_period_params = slope_midpoint(int_df, mean(int_df$frequencies))

  output = unname(c(long_period_params$Slope, long_period_params$Midpoint,
                    int_period_params$Slope, int_period_params$Midpoint,
                    short_period_params$Slope, short_period_params$Midpoint))
  names(output) = c("long_slope", "long_midpoint", "int_slope", "int_midpoint",
                    "short_slope", "short_midpoint")
  return(output)
}


