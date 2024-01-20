#' Class for packaging a log-periodogram with its corresponding frequencies
#'
#' @details Object contains both the log-periodogram and frequencies as
#' corresponding vectors
#'
#' @param log_periodogram, vector containing log-transformed periodogram ordinates
#' @param frequencies, vector containing frequency arguments corresponding to the
#' transformed periodogram values in "log_periodogram"
#'
#' @return object with appropriate "Periodogram" class and components containing
#' both the transformed periodogram and its frequency arguments
#'
new_pgram <- function(log_periodogram, frequencies){
  structure(list(lpgram = log_periodogram,
                 freqs = frequencies),
            class = "Periodogram")
}

#' Class for representing 6DF piece-wise linear model periodogram summary
#'
#' @details Contains all slopes and midpoints across 3 periodicity bands of
#' interest (greater than a day, a day to 150 minutes, shorter than 150 minutes)
#'
#' @param long_params, named vector containing slope and midpoint over the
#' long-periodicity band of periodogram arguments
#' @param int_params, named vector containing slope and midpoint over the
#' intermediate-periodicity band of periodogram arguments
#' @param short_params, named vector containing slope and midpoint over the
#' short-periodicity band of periodogram arguments
#'
#' @return object with appropriate "6DF_summary" class and each summary
#' measure contained in its own component
#'
new_6DF <- function(long_params, int_params, short_params){
  structure(list(long_slope = long_params$Slope,
                 long_midpoint = long_params$Midpoint,
                 int_slope = int_params$Slope,
                 int_midpoint = int_params$Midpoint,
                 short_slope = short_params$Slope,
                 short_midpoint = short_params$Midpoint),
            class = "6DF_summary")
}
