% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main function.R
\name{bin_distribution}
\alias{bin_distribution}
\title{bin_distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{the total number of the trials}

\item{prob}{the probability of success every time}
}
\value{
an object of class \code{"bindis"}
}
\description{
Create an object of class \code{"bindis"}
}
\examples{
bin_distribution(trials = 5, prob = 0.5)
}
