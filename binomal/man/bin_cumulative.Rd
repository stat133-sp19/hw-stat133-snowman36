% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main function.R
\name{bin_cumulative}
\alias{bin_cumulative}
\title{bin_cumulative}
\usage{
bin_cumulative(trials, prob)
}
\arguments{
\item{trials}{the total number of the trials}

\item{prob}{the probability of success every time}
}
\value{
an object of class \code{"bindis"}
}
\description{
Create an object of class \code{"bincum"}
}
\examples{
bin_cumulative(trials = 5, prob = 0.5)
}
