% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period_cd.R
\name{period_cd}
\alias{period_cd}
\alias{cm_to_fm}
\alias{cm_to_cq}
\alias{cm_to_fq}
\alias{cm_to_cy}
\alias{cm_to_fy}
\alias{cq_to_fq}
\alias{cq_to_fy}
\alias{cq_to_cy}
\alias{fm_to_cm}
\alias{fm_to_cq}
\alias{fm_to_fq}
\alias{fm_to_cy}
\alias{fm_to_fy}
\alias{fq_to_cq}
\alias{fq_to_cy}
\alias{fq_to_fy}
\alias{periods}
\title{Numeric codes for periods}
\usage{
period_cd(x, period = "fy")

cm_to_fm(f)

cm_to_cq(f)

cm_to_fq(f)

cm_to_cy(f)

cm_to_fy(f)

cq_to_fq(f)

cq_to_fy(f)

cq_to_cy(f)

fm_to_cm(f)

fm_to_cq(f)

fm_to_fq(f)

fm_to_cy(f)

fm_to_fy(f)

fq_to_cq(f)

fq_to_cy(f)

fq_to_fy(f)

periods(x, origin = Sys.Date(), interval = 6, period_nm = FALSE)
}
\arguments{
\item{x}{\code{Date}, \code{POSIXct} or \code{POSIXt}. \code{numeric} and \code{integer} for \code{periods()}}

\item{period}{Options are \code{"fy"} (default), \code{"fy"}, \code{"cy"}, \code{"fq"}, \code{"cq"}, \code{"fm"}, \code{"cm"}}

\item{f}{Time code. Output of \code{period_cd()}}

\item{origin}{Start of the period. Must be the same object type as \code{x}}

\item{interval}{Interval between periods.}

\item{period_nm}{If \code{TRUE}, returns the first point (day, time or number) of the period}
}
\value{
\code{numeric}. \code{Date}, \code{POSIXct} or \code{POSIXt} for \code{periods()}
}
\description{
Convenient time codes for analysis.
}
\details{
\code{cm_to_fm()} - Convert calendar month codes to financial month codes

\code{cm_to_cq()} - Convert calendar month codes to calendar quarter codes

\code{cm_to_fq()} - Convert calendar month codes to financial quarter codes

\code{cm_to_cy()} - Convert calendar month codes to calendar year codes

\code{cm_to_fy()} - Convert calendar month codes to financial year codes

\code{cq_to_fq()} - Convert calendar quarter codes to financial quarter codes

\code{cq_to_cy()} - Convert calendar quarter codes to financial year codes

\code{cq_to_cy()} - Convert calendar quarter codes to calendar year codes

\code{fm_to_cm()} - Convert financial month codes to calendar month codes

\code{fm_to_cq()} - Convert calendar month codes to calendar quarter codes

\code{fm_to_fq()} - Convert calendar month codes to financial quarter codes

\code{fm_to_cy()} - Convert financial month codes to calendar year codes

\code{fm_to_fy()} - Convert financial month codes to financial year codes

\code{fq_to_cq()} - Convert financial quarter codes to calendar quarter codes

\code{fq_to_cy()} - Convert financial quarter codes to calendar year codes

\code{fq_to_fy()} - Convert financial quarter codes to financial year codes

Convert dates to custom intervals
}
\examples{
dt <- Sys.Date()
period_cd(dt)
period_cd(dt,"cy")
period_cd(dt,"fq")
period_cd(dt,"cq")
period_cd(dt,"fm")
period_cd(dt,"cm")
period_cd(dt,"fd")
period_cd(dt,"cq")
cm_to_fm("201204")
cm_to_fm("201201")
cm_to_cq("201201")
cm_to_cq("201211")
cm_to_fq("201201")
cm_to_fq("201211")
cm_to_cy("201204")
cm_to_cy("201201")
cm_to_fy("201204")
cm_to_fy("201201")
cq_to_fq("20124")
cq_to_fq("20121")
cq_to_fy("20124")
cq_to_fy("20121")
cq_to_cy("20124")
cq_to_cy("20121")
fm_to_cm("201204")
fm_to_cm("201201")
fm_to_cq("201201")
fm_to_cq("201211")
fm_to_fq("201201")
fm_to_fq("201211")
fm_to_cy("201204")
fm_to_cy("201201")
fm_to_fy("201204")
fm_to_fy("201201")
fq_to_cq("20124")
fq_to_cq("20121")
fq_to_cy("20124")
fq_to_cy("20121")
fq_to_fy("20124")
fq_to_fy("20121")
date <- as.Date("2019-01-05")
periods(x = date, origin = date)
periods(x = Sys.Date(), origin = date)
periods(x = Sys.Date(), origin = date, period_nm = TRUE)
}
