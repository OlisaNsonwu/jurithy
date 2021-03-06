% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rolling_value.R
\name{rolling_value}
\alias{rolling_value}
\title{Rolling function of a numeric values}
\usage{
rolling_value(val, by = 12, func = sum)
}
\arguments{
\item{val}{Ordered vector}

\item{by}{Increment}

\item{func}{Function}
}
\value{
Vector of he same class as \code{val}
}
\description{
Rolling function of a numeric values
}
\examples{

rolling_value(1:20, by = 5)
rolling_value(1:20, by = 10)
rolling_value(1:20, by = 10, func = mean)

}
