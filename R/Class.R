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
