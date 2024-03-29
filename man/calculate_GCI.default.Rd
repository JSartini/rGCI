% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GCI.R
\name{calculate_GCI.default}
\alias{calculate_GCI.default}
\title{Catch attempt to calculate GCI using unsupported class}
\usage{
\method{calculate_GCI}{default}(x, weights, means, stddevs, ...)
}
\arguments{
\item{x, }{Object of class other than "6DF_summary" or dataframe}

\item{weights, }{numeric vector of weights used to form the GCI linear combination}

\item{means, }{numeric vector of mean values for the 6 degree of freedom
approximation, subtracted away before calculation of GCI}

\item{stddevs, }{numeric vector of standard deviations for each of the values
within the 6 degree of freedom approximation, used for scaling before GCI calculation}

\item{..., }{additional arguments/settings (not required here)}
}
\description{
Catch attempt to calculate GCI using unsupported class
}
