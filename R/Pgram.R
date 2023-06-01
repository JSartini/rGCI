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
#' @return Periodogram, list with entries "pgram" - vector of log-periodogram values
#' and "freqs" - vector of frequencies corresponding to those values
#'
#' @export
#'
pgram <- function(ts, arg, options = default_pgram()){
  # Determine frequency of observation
  pos_times = c(5,15)
  differ = as.numeric(difftime(arg[2], arg[1], units = "mins"))
  interval = which.min(abs(differ-c(5, 15)))
  interval = pos_times[interval]

  # Constants
  per_hour = 60/interval
  req_num = 14*24*per_hour
  daily_spec = per_hour*24
  hourly_spec = per_hour

  difference = as.numeric(difftime(arg, c(tail(arg,1), arg[length(arg)]), units = 'mins'))

  # Pad the data
  num_add = (difference %/% interval) - 1
  indices = which(num_add > 0)
  for_analysis = ts

  for(idx in indices){
    # Too large of gaps can result in biased estimates
    if(num_add[idx] > options[["maxgap"]]*daily_spec){
      break
    }

    for_analysis = append(for_analysis, rep(0, num_add[idx]),
                          after = idx-1)
  }

  # If there is not enough data, the longer signals will be more difficult to discern
  if(length(for_analysis) < (options[["mindata"]]/options[["days"]])*req_num){
    next
  }

  spec_data = rep(0, req_num)
  spec_data[1:length(for_analysis)] = for_analysis

  # Trim data to avoid issues with sensor warmup and degradation
  spec_data = spec_data[(daily_spec+1):(req_num - daily_spec)]

  # Calculate relevant metrics - spline regression coefficients
  analysis = spec.pgram(spec_data, spans = options[["spans"]],
                        demean = TRUE, plot = FALSE)

  pgram = log(analysis$spec)
  freqs = analysis$freq

  return(list(pgram = pgram, freqs = freqs))
}
