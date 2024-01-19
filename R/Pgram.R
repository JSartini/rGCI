#' Return reasonable defaults for CGM log-periodogram calculation
#'
#' @details Defaults used in paper deriving the GCI (citation)
#'
#' @return options, list containing reasonable default options for pgram(), allowing
#' for user customization from baseline
#'
#' @export
#'
default_pgram <- function(){
  return(list(days = 14,
              maxgap = 2,
              mindata = 7,
              strim = 1,
              etrim = 1,
              spans = c(3,5,7)))
}

#' Produce required constants for periodogram calculation
#'
#' @details Calculates expected number of observations per-hour, per-wear, and
#' per-day
#'
#' @param interval, time (in minutes) between CGM observations
#' @param wear_length, number of days users are expected to wear the device, set
#' by default to 14
#'
#' @return constant_list, named list of the required constants
#'
calc_constants <- function(interval, wear_length = 14){
  ph = 60/interval
  pd = ph*24
  pw = pd*wear_length
  constant_list = list(per_day = pd, per_wear = pw)
  return(constant_list)
}

#' Pad the CGM time series with zeros
#'
#' @details Ensures that glucose observations are appropriately and evenly
#' spaced, accounting for missing data within and potentially at the end of the
#' time series. Required step for accurate estimates of the spectral density
#' from the periodogram.
#'
#' @param ts, numeric time series of CGM data as a numeric vector
#' @param arg, POSIXct vector of timestamps for CGM time series
#' @param options, list containing relevant settings for periodogram calculation,
#' including number of expected days in the data ("days"), maximum gap in the
#' data allowable ("maxgap", in days), minimum data for analysis ("mindata" in days),
#' amount of start and end data to trim ("strim" and "etrim" both in days),
#' and odd-value spans of Daniel smoothing kernels ("spans")
#' @param consts, list of observation frequency constants
#' @param interval, time (in minutes) between CGM observations
#'
#' @return spec_data, vector containing the original CGM time series with
#' appropriate zero-padding (assuming sufficient data and continuity)
#'
pad_for_pgram <- function(ts, arg, options, consts, interval){
  diffs = as.numeric(difftime(arg[-1], arg[-length(arg)], units = 'mins'))

  # Pad the data
  num_add = c(1, (diffs %/% interval)) - 1
  indices = which(num_add > 0)
  for_analysis = ts

  for(idx in indices){
    # Too large of gaps can result in biased estimates
    if(num_add[idx] > options$maxgap*consts$per_day){
      stop("Excessively long gap found in the data")
    }

    for_analysis = append(for_analysis, rep(0, num_add[idx]),
                          after = idx-1)
  }

  # If there is not enough data, the longer signals will be more difficult to discern
  if(length(for_analysis) < (options$mindata/options$days)*consts$per_wear){
    stop("Insufficient data length")
  }

  spec_data = rep(0, consts$per_wear)
  spec_data[1:length(for_analysis)] = for_analysis

  return(spec_data)
}


#' Calculate CGM periodogram for a given time series and argument
#'
#' @details This function calculates the CGM log-periodogram using appropriate settings for this type of data
#'
#' @param ts, numeric time series of CGM data as a numeric vector
#' @param arg, POSIXct vector of timestamps for CGM time series
#' @param options, list containing relevant settings for periodogram calculation,
#' including number of expected days in the data ("days"), maximum gap in the
#' data allowable ("maxgap", in days), minimum data for analysis ("mindata" in days),
#' amount of start and end data to trim ("strim" and "etrim" both in days),
#' and odd-value spans of Daniel smoothing kernels ("spans"). This argument is
#' required, and a set of reasonable defaults is provided by the default_pgram()
#' function.
#'
#' @return list with entries "pgram" - vector of log-periodogram values,
#' "freqs" - vector of frequencies (min ^-1) corresponding to those values
#'
#' @export
#'
pgram <- function(ts, arg, options = default_pgram()){
  # Determine frequency of observation
  pos_times = c(1,5,15)
  differ = as.numeric(difftime(arg[2], arg[1], units = "mins"))
  interval = which.min(abs(differ-pos_times))
  interval = pos_times[interval]

  consts = calc_constants(interval, options$days)

  spec_data = pad_for_pgram(ts, arg, options, consts, interval)
  # Trim for sensor warmup and degradation
  spec_data = spec_data[(consts$per_day+1):(consts$per_wear -consts$per_day)]

  analysis = spec.pgram(spec_data, spans = options$spans,
                        demean = TRUE, plot = FALSE)

  return(list(pgram = log(analysis$spec), freqs = analysis$freq/interval))
}
