% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_drate.R
\name{estimate_drate}
\alias{estimate_drate}
\title{Estimate the drate}
\usage{
estimate_drate(y, x = NULL)
}
\arguments{
\item{y}{The y values}

\item{x}{The x values (if NULL, it will be from 1 to length of y)}
}
\value{
The drate
}
\description{
Estimate the drate
}
\examples{
estimate_drate(y = runif(100, 2, 5))
}
