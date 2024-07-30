#' @title Numeric codes for periods
#'
#' @description Convenient time codes for analysis.
#'
#' @param x \code{Date}, \code{POSIXct} or \code{POSIXt}. \code{numeric} and \code{integer} for \code{periods()}
#' @param period Options are \code{"fy"} (default), \code{"fy"}, \code{"cy"}, \code{"fq"}, \code{"cq"}, \code{"fm"}, \code{"cm"}
#' @param origin Start of the period. Must be the same object type as \code{x}
#' @param interval Interval between periods.
#' @param period_nm If \code{TRUE}, returns the first point (day, time or number) of the period
#' @param f Time code. Output of \code{period_cd()}
#' @return \code{numeric}. \code{Date}, \code{POSIXct} or \code{POSIXt} for \code{periods()}
#' @aliases period_cd
#' @examples
#' dt <- Sys.Date()
#' period_cd(dt)
#' period_cd(dt,"cy")
#' period_cd(dt,"fq")
#' period_cd(dt,"cq")
#' period_cd(dt,"fm")
#' period_cd(dt,"cm")
#' period_cd(dt,"fd")
#' period_cd(dt,"cq")
#' @export

period_cd <- function(x, period = "fy"){
  if(!any(class(x) %in% c("Date","POSIXct","POSIXt"))) stop("'x' must be a 'Date', 'POSIXct' or 'POSIXt'!")
  if(length(period) != 1) stop("`period` must have a length of 1!")
  if(!period %in% c("fy","cy","fq","cq","fm","cm","fd","cd")) stop("'period' must be 'fy', 'cy', 'fq', 'cq', 'fm', 'cm', 'fd' or 'cd'")

  m <- as.numeric(format(x, "%m"))
  y <- as.numeric(format(x, "%Y"))
  d <-  as.numeric(format(x, "%d"))
  x <- (y * 100) + m

  if(period %in% c("fy", "cy", "fq", "cq", "fm")){
    func <- get(paste0("cm_to_", period))
    x <- func(x)
  }else if (period == "cd"){
    x <- (x * 100) + d
  }else if (period == "fd"){
    x <- (cm_to_fm(x) * 100) + d
  }
  x
}

#' @rdname period_cd
#' @details
#' \code{cm_to_fm()} - Convert calendar month codes to financial month codes
#' @examples
#' cm_to_fm("201204")
#' cm_to_fm("201201")
#' @export
cm_to_fm <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  m6cd <- all(nchar(f) == 6)
  f <- as.numeric(f)
  if(m6cd){
    y <- as.integer(f/100)
    m <- f - (y * 100)
  }else{
    m <- f
  }
  m2 <- c(10:12,1:9)[m]
  if(!m6cd){
    return(m2)
  }
  x <- (y * 100) + m2
  lgk <- m2 >= 10
  x[lgk] <- ((y[lgk] - 1) * 100) + m2[lgk]
  x
}

#' @rdname period_cd
#' @details
#'  \code{cm_to_cq()} - Convert calendar month codes to calendar quarter codes
#' @examples
#' cm_to_cq("201201")
#' cm_to_cq("201211")
#' @export
cm_to_cq <- function(f){
  y <- as.integer(f/100)
  m <- f - (y * 100)
  if(!all(nchar(f) == 6 & as.numeric(m) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  f <- as.integer(f)
  q <- rep(1:4, rep(3, 4))
  q <- (y * 10) + q[m]
  q
}

#' @rdname period_cd
#' @details
#'  \code{cm_to_fq()} - Convert calendar month codes to financial quarter codes
#' @examples
#' cm_to_fq("201201")
#' cm_to_fq("201211")
#' @export
cm_to_fq <- function(f){
  y <- as.integer(f/100)
  m <- f - (y * 100)
  if(!all(nchar(f) == 6 & as.numeric(m) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  cm_to_cq(cm_to_fm(f))
}

#' @rdname period_cd
#' @details
#' \code{cm_to_cy()} - Convert calendar month codes to calendar year codes
#' @examples
#' cm_to_cy("201204")
#' cm_to_cy("201201")
#' @export
cm_to_cy <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(f/100)
}

#' @rdname period_cd
#' @details
#' \code{cm_to_fy()} - Convert calendar month codes to financial year codes
#' @examples
#' cm_to_fy("201204")
#' cm_to_fy("201201")
#' @export
cm_to_fy <- function(f){
  if(all(!nchar(f) %in% c(6))) stop("Incorrect codes. 6 Digit Code required!")
  as.integer(cm_to_fm(f)/100)
}

#' @rdname period_cd
#' @details
#' \code{cq_to_fq()} - Convert calendar quarter codes to financial quarter codes
#' @examples
#' cq_to_fq("20124")
#' cq_to_fq("20121")
#' @export
cq_to_fq <- function(f){
  f <- as.integer(f)
  y <- as.integer(f/10)
  q <- f - (y * 10)
  if(!all(nchar(f) == 5 & q %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  x <- (y * 10) + (c(4,1,2,3)[q])
  lgk <- q == 1
  x[lgk] <- ((y[lgk] - 1) * 10) + 4
  x
}

#' @rdname period_cd
#' @details
#' \code{cq_to_cy()} - Convert calendar quarter codes to financial year codes
#' @examples
#' cq_to_fy("20124")
#' cq_to_fy("20121")
#' @export
cq_to_fy <- function(f){
  f <- as.integer(f)
  y <- as.integer(f/10)
  q <- f - (y * 10)
  if(!all(nchar(f) == 5 & q %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(cq_to_fq(f)/10)
}

#' @rdname period_cd
#' @details
#' \code{cq_to_cy()} - Convert calendar quarter codes to calendar year codes
#' @examples
#' cq_to_cy("20124")
#' cq_to_cy("20121")
#' @export
cq_to_cy <- function(f){
  f <- as.integer(f)
  y <- as.integer(f/10)
  q <- f - (y * 10)
  if(!all(nchar(f) == 5 & q %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(f/10)
}

#' @rdname period_cd
#' @details
#' \code{fm_to_cm()} - Convert financial month codes to calendar month codes
#' @examples
#' fm_to_cm("201204")
#' fm_to_cm("201201")
#' @export
fm_to_cm <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  m6cd <- all(nchar(f) == 6)
  f <- as.numeric(f)
  if(m6cd){
    y <- as.integer(f/100)
    m <- f - (y * 100)
  }else{
    m <- f
  }

  m2 <- c(4:12,1:3)[m]
  if(!m6cd){
    return(m2)
  }
  x <- (y * 100) + m2
  lgk <- m >= 10

  x[lgk] <- ((y[lgk] + 1) * 100) + m2[lgk]
  x
}

#' @rdname period_cd
#' @details
#'  \code{fm_to_cq()} - Convert calendar month codes to calendar quarter codes
#' @examples
#' fm_to_cq("201201")
#' fm_to_cq("201211")
#' @export
fm_to_cq <- function(f){
  cm_to_cq(fm_to_cm(f))
}

#' @rdname period_cd
#' @details
#'  \code{fm_to_fq()} - Convert calendar month codes to financial quarter codes
#' @examples
#' fm_to_fq("201201")
#' fm_to_fq("201211")
#' @export
fm_to_fq <- cm_to_cq

#' @rdname period_cd
#' @details
#' \code{fm_to_cy()} - Convert financial month codes to calendar year codes
#' @examples
#' fm_to_cy("201204")
#' fm_to_cy("201201")
#' @export
fm_to_cy <- function(f){
  cm_to_cy(fm_to_cm(f))
}

#' @rdname period_cd
#' @details
#' \code{fm_to_fy()} - Convert financial month codes to financial year codes
#' @examples
#' fm_to_fy("201204")
#' fm_to_fy("201201")
#' @export
fm_to_fy <- cm_to_cy

#' @rdname period_cd
#' @details
#'  \code{fq_to_cq()} - Convert financial quarter codes to calendar quarter codes
#' @examples
#' fq_to_cq("20124")
#' fq_to_cq("20121")
#' @export
fq_to_cq <- function(f){
  f <- as.integer(f)
  y <- as.integer(f/10)
  q <- f - (y * 10)
  if(!all(nchar(f) == 5 & q %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  x <- (y * 10) + (c(2,3,4,1)[q])
  lgk <- q == 4
  x[lgk] <- ((y[lgk] + 1) * 10) + 1
  x
}

#' @rdname period_cd
#' @details
#' \code{fq_to_cy()} - Convert financial quarter codes to calendar year codes
#' @examples
#' fq_to_cy("20124")
#' fq_to_cy("20121")
#' @export
fq_to_cy <- function(f){
  cq_to_cy(fq_to_cq(f))
}

#' @rdname period_cd
#' @details
#' \code{fq_to_fy()} - Convert financial quarter codes to financial year codes
#' @examples
#' fq_to_fy("20124")
#' fq_to_fy("20121")
#' @export
fq_to_fy <- cq_to_cy


#' @rdname period_cd
#' @details
#' Convert dates to custom intervals
#' @examples
#' date <- as.Date("2019-01-05")
#' periods(x = date, origin = date)
#' periods(x = Sys.Date(), origin = date)
#' periods(x = Sys.Date(), origin = date, period_nm = TRUE)
#' @export
periods <- function(x, origin = Sys.Date(), interval = 6, period_nm = FALSE){
  if(any(!atomic_content(list(x, origin, interval, period_nm)))) stop(paste0(paste0("`", c("x", "origin", "interval", "period_nm"), "`", collapse = ", "), " must be actomic vectors!"))
  if(!any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt", "numeric", "integer"))) stop(paste0("`x` must be", paste0("\"", c("Date", "POSIXct", "POSIXlt", "POSIXt", "numeric", "integer"), "\"", collapse = ", "), " objects!"))
  if(!all(class(x) == class(origin))) stop("`x` and `origin` must have the same object types !")
  if(!any(class(interval) %in% c("numeric", "integer"))) stop(paste0("`interval` must be", paste0("\"", c("numeric", "integer"), "\"", collapse = ", "), " objects!"))
  if(!any(class(period_nm) %in% c("logical"))) stop(paste0("`period_nm` must be a", paste0("\"", c("logical"), "\"", collapse = ", "), " object!"))
  y <- floor((as.numeric(x) - as.numeric(origin))/interval) + 1
  if (period_nm == FALSE){
    y
  }else{
    seq(origin, max(x), by = interval)[y]
  }
}
