% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period_days.R
\name{period_days}
\alias{period_days}
\alias{days_in_cm}
\alias{days_in_fm}
\alias{days_in_cq}
\alias{days_in_fq}
\alias{days_in_cy}
\alias{days_in_fy}
\title{Number of days in a period}
\usage{
days_in_cm(x)

days_in_fm(x)

days_in_cq(x)

days_in_fq(x)

days_in_cy(x)

days_in_fy(x)
}
\arguments{
\item{x}{Time code. Output of \code{period_cd()}}
}
\value{
\code{numeric}.
}
\description{
Number of days in a period
}
\examples{
days_in_cm(202002)
days_in_fm(202002)
days_in_cq(20202)
days_in_fq(20202)
days_in_cy(2020)
days_in_fy(2020)

}
