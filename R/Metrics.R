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
  long_idxs = which(periods >= 1440)
  int_idxs = which(periods < 1440 & periods >= 250)
  short_idxs = which(periods < 250)

  long_inputs = fit_df %>% slice(long_idxs)
  mid = mean(long_inputs$frequencies)
  long_fit = lm(spectra ~ frequencies, data = long_inputs)
  lslope = long_fit$coefficients[2]
  lmidpoint = predict(long_fit, newdata = data.frame(frequencies = c(mid)))

  med_inputs = fit_df %>% slice(int_idxs)
  mid = median(med_inputs$frequencies)
  med_fit = lm(spectra ~ frequencies, data = med_inputs)
  islope = med_fit$coefficients[2]
  imidpoint = predict(med_fit, newdata = data.frame(frequencies = c(mid)))

  short_inputs = fit_df %>% slice(short_idxs)
  mid = mean(short_inputs$frequencies)
  short_fit = lm(spectra ~ frequencies, data = short_inputs)
  sslope = short_fit$coefficients[2]
  smidpoint = predict(short_fit, newdata = data.frame(frequencies = c(mid)))

  return(c(long_slope = lslope, long_midpoint = lmidpoint,
           int_slope = islope, int_midpoint = imidpoint,
           short_slope = sslope, short_midpoint = smidpoint))
}


