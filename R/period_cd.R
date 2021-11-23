#' @title Numeric codes for periods
#'
#' @description Convenient time codes for analysis.
#'
#' @param x \code{Date}, \code{POSIXct} or \code{POSIXt}. \code{numeric} and \code{integer} for \code{periods()}
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
  m_l <- as.numeric(format(x, "%m"))
  m <- as.numeric(m_l)
  y <- as.numeric(format(x, "%Y"))
  d_l <- format(x, "%d")
  d <- as.numeric(d_l)
  if(period=="fy"){
    x <- ifelse(m %in% 1:3,
                paste0(y - 1, substr(y, 3, 4)),
                paste0(y, substr(y + 1, 3, 4)))
  }else if(period=="cy"){
    x <- y
  }else if(period=="fq"){
    qts <- c(rep(4, 3), rep(1, 3), rep(2, 3), rep(3, 3))
    x <- ifelse(m %in% 1:3,
                paste0(y-1, qts[m]),
                paste0(y, qts[m]))
  }else if(period=="cq"){
    qts <- c(rep(1,3), rep(2,3), rep(3,3), rep(4,3))
    x <- paste0(y, qts[m])
  }else if(period=="fm"){
    mns <- formatC(c(10:12, 1:9), width = 2, format = "d", flag = "0")
    x <- paste0(ifelse(m %in% 1:3, y - 1, y), mns[m])
  }
  else if(period=="cm"){
    x <- format(x, "%Y%m")
  }else if(period=="cd"){
    x <- format(x, "%Y%m%d")
  }else if(period=="fd"){
    mns <- formatC(c(10:12, 1:9), width = 2, format = "d", flag = "0")
    x <- paste0(ifelse(m %in% 1:3, y - 1, y), mns[m], d_l)
  }

  as.numeric(x)
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

  y <- as.numeric(f)
  y[nchar(f) == 6] <- as.numeric(substr(f[nchar(f) == 6], 5, 6))

  ord <- c(10:12,1:9)
  m <- y <- ord[y]

  n_pad <- function(x){
    if(length(x) == 0){
      x
    }else{
      formatC(as.numeric(x), width = 2, flag = 0, format = "fg")
    }
  }
  y[nchar(f) == 6 & m %in% 10:12] <- paste0(as.numeric(substr(f[nchar(f) == 6 & m %in% 10:12], 1, 4)) - 1,
                                            n_pad(y[nchar(f) == 6 & m %in% 10:12]))

  y[nchar(f) == 6 & !m %in% 10:12] <- paste0(as.numeric(substr(f[nchar(f) == 6 & !m %in% 10:12], 1, 4)),
                                             n_pad(y[nchar(f) == 6 & !m %in% 10:12]))
  as.integer(y)
}

#' @rdname period_cd
#' @details
#'  \code{cm_to_cq()} - Convert calendar month codes to calendar quarter codes
#' @examples
#' cm_to_cq("201201")
#' cm_to_cq("201211")
#' @export
cm_to_cq <- function(f){
  if(!all(nchar(f) == 6 & as.numeric(substr(f, 5,6)) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- sort(rep(1:4,3))
  q <-paste0(substr(f, 1,4), q[as.numeric(substr(f, 5,6))])
  as.integer(q)
}

#' @rdname period_cd
#' @details
#'  \code{cm_to_fq()} - Convert calendar month codes to financial quarter codes
#' @examples
#' cm_to_fq("201201")
#' cm_to_fq("201211")
#' @export
cm_to_fq <- function(f){
  if(!all(nchar(f) == 6 & as.numeric(substr(f, 5,6)) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  f <- cm_to_fm(f)
  q <- sort(rep(1:4,3))
  q <- paste0(substr(f, 1,4), q[as.numeric(substr(f, 5,6))])
  as.integer(q)
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
  as.integer(substr(f, 1, 4))
}

#' @rdname period_cd
#' @details
#' \code{cm_to_fy()} - Convert calendar month codes to financial year codes
#' @examples
#' cm_to_fy("201204")
#' cm_to_fy("201201")
#' @export
cm_to_fy <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(cm_to_fm(f), 1, 4))
}

#' @rdname period_cd
#' @details
#' \code{cq_to_fq()} - Convert calendar quarter codes to financial quarter codes
#' @examples
#' cq_to_fq("20124")
#' cq_to_fq("20121")
#' @export
cq_to_fq <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- as.numeric(substr(f, 5, 5))
  y <- as.numeric(substr(f, 1, 4))
  x <- paste0(y, q - 1)
  x[q == 1] <- paste0(y[q == 1] - 1, 4)
  as.integer(x)
}

#' @rdname period_cd
#' @details
#' \code{cq_to_cy()} - Convert calendar quarter codes to financial year codes
#' @examples
#' cq_to_fy("201204")
#' cq_to_fy("201201")
#' @export
cq_to_fy <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(cq_to_fq(f), 1, 4))
}

#' @rdname period_cd
#' @details
#' \code{cq_to_cy()} - Convert calendar quarter codes to calendar year codes
#' @examples
#' cq_to_cy("201204")
#' cq_to_cy("201201")
#' @export
cq_to_cy <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(f, 1, 4))
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

  y <- as.numeric(f)
  y[nchar(f) == 6] <- as.numeric(substr(f[nchar(f) == 6], 5, 6))

  ord <- c(4:12,1:3)
  m <- y <- ord[y]

  n_pad <- function(x){
    if(length(x) == 0){
      x
    }else{
      formatC(as.numeric(x), width = 2, flag = 0, format = "fg")
    }
  }
  y[nchar(f) == 6 & m %in% 1:3] <- paste0(as.numeric(substr(f[nchar(f) == 6 & m %in% 1:3], 1, 4)) + 1,
                                          n_pad(y[nchar(f) == 6 & m %in% 1:3]))

  y[nchar(f) == 6 & !m %in% 1:3] <- paste0(as.numeric(substr(f[nchar(f) == 6 & !m %in% 1:3], 1, 4)),
                                           n_pad(y[nchar(f) == 6 & !m %in% 1:3]))
  as.integer(y)
}

#' @rdname period_cd
#' @details
#'  \code{fm_to_cq()} - Convert calendar month codes to calendar quarter codes
#' @examples
#' fm_to_cq("201201")
#' fm_to_cq("201211")
#' @export
fm_to_cq <- function(f){
  if(!all(nchar(f) == 6 & as.numeric(substr(f, 5,6)) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  f <- fm_to_cm(f)
  q <- sort(rep(1:4,3))
  q <- paste0(substr(f, 1,4), q[as.numeric(substr(f, 5,6))])
  as.integer(q)
}

#' @rdname period_cd
#' @details
#'  \code{fm_to_fq()} - Convert calendar month codes to financial quarter codes
#' @examples
#' fm_to_fq("201201")
#' fm_to_fq("201211")
#' @export
fm_to_fq <- function(f){
  if(!all(nchar(f) == 6 & as.numeric(substr(f, 5,6)) %in% 1:12)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- sort(rep(1:4,3))
  q <- paste0(substr(f, 1,4), q[as.numeric(substr(f, 5,6))])
  as.integer(q)
}

#' @rdname period_cd
#' @details
#' \code{fm_to_cy()} - Convert financial month codes to calendar year codes
#' @examples
#' cm_to_cy("201204")
#' cm_to_cy("201201")
#' @export
fm_to_cy <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(fm_to_cm(f), 1, 4))
}

#' @rdname period_cd
#' @details
#' \code{fm_to_fy()} - Convert financial month codes to financial year codes
#' @examples
#' fm_to_fy("201204")
#' fm_to_fy("201201")
#' @export
fm_to_fy <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(f, 1, 4))
}

#' @rdname period_cd
#' @details
#'  \code{fq_to_cq()} - Convert financial quarter codes to calendar quarter codes
#' @examples
#' fq_to_cq("20124")
#' fq_to_cq("20121")
#' @export
fq_to_cq <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- as.numeric(substr(f, 5, 5))
  y <- as.numeric(substr(f, 1, 4))
  x <- paste0(y, q + 1)
  x[q == 4] <- paste0(y[q == 4] + 1, 1)
  as.integer(x)
}

#' @rdname period_cd
#' @details
#' \code{fq_to_cy()} - Convert financial quarter codes to calendar year codes
#' @examples
#' fq_to_cy("201204")
#' fq_to_cy("201201")
#' @export
fq_to_cy <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(fq_to_cq(f), 1, 4))
}

#' @rdname period_cd
#' @details
#' \code{fq_to_fy()} - Convert financial quarter codes to financial year codes
#' @examples
#' fq_to_fy("201204")
#' fq_to_fy("201201")
#' @export
fq_to_fy <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  as.integer(substr(f, 1, 4))
}


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
