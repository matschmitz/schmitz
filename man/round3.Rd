% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataWranglingCleaning.R
\name{round3}
\alias{round3}
\title{Round 3}
\usage{
round3(x)
}
\arguments{
\item{x}{a number or a numeric vector}
}
\value{
a string or a character vector
}
\description{
Rounds the number to three decimal points.
If the value < .001 than a label "<.001" is used.
Removes leading zeros.
}
\examples{
round3(c(0.033, 0.1, 0.0002))
}
