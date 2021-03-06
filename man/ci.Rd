% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_intervals.R
\name{ci}
\alias{ci}
\alias{ci_prop_diff}
\alias{ci_rate_ratio}
\title{Confidence intervals for a difference between two proportions}
\usage{
ci_prop_diff(p1, n1, p2, n2, sig_level = 0.95)

ci_rate_ratio(r1, n1, r2, n2, sig_level = 0.95)
}
\arguments{
\item{p1}{Proportion 1}

\item{n1}{Sample size 1}

\item{p2}{Proportion 2}

\item{n2}{Sample size 2}

\item{r1}{Incidence rate 1}

\item{r2}{Incidence rate 2}
}
\value{
\code{numeric}
}
\description{
Confidence intervals for a difference between two proportions
}
\examples{

ci_prop_diff(0.56, 80, 0.76, 80)


ci_rate_ratio(4.1, 157, 4.4, 178)

}
