#' @title Numeric codes for periods
#'
#' @description Convenient time codes for analysis.
#'
#' @param x \code{Date}, \code{POSIXct} or \code{POSIXt}.
#' @param f Time code. Output of \code{period_cd()}.
#' @return \code{numeric}
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
#' Convert calendar month codes to financial month codes
#' @export
cm_to_fm <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")

  y <- ifelse(nchar(f)==6, as.numeric(substr(f,5,6)), as.numeric(f))

  fm_ord <- c(10:12,1:9)
  cm_ord <- unlist(lapply(1:12, function(x){which(fm_ord==x)}))
  z <- fm_ord[y]

  z <- ifelse(nchar(f)==6,
              ifelse(y %in% 1:3,
                     as.numeric(paste0(as.numeric(substr(f,1,4))-1, formatC(z, width= 2, flag=0, format = "fg"))),
                     as.numeric(paste0(substr(f,1,4), formatC(z, width= 2, flag=0, format = "fg")))),
              z)

  z
}

#' @rdname period_cd
#' @details
#' Convert financial month codes to calendar month codes
#' @export
fm_to_cm <- function(f){
  if(any(!nchar(f) %in% c(1:2, 6))) stop("Incorrect codes. See the output of `period_cd` for the required format!")

  y <- ifelse(nchar(f)==6, as.numeric(substr(f,5,6)), as.numeric(f))

  fm_ord <- c(10:12,1:9)
  cm_ord <- unlist(lapply(1:12, function(x){which(fm_ord==x)}))
  z <- cm_ord[y]

  z <- ifelse(nchar(f)==6,
              ifelse(z %in% 1:3,
                     as.numeric(paste0(as.numeric(substr(f,1,4))+1, formatC(z, width= 2, flag=0, format = "fg"))),
                     as.numeric(paste0(substr(f,1,4), formatC(z, width= 2, flag=0, format = "fg")))),
              z)

  z
}

#' @rdname period_cd
#' @details
#' Convert calendar month codes to financial month codes
#' @export
cq_to_fq <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- as.numeric(substr(f, 5, 5))
  y <- as.numeric(substr(f, 1, 4))
  ifelse(q == 1, paste0(y - 1, 4), paste0(y, q - 1))
}

#' @rdname period_cd
#' @details
#' Convert financial month codes to calendar month codes
#' @export
fq_to_cq <- function(f){
  if(!all(nchar(f) == 5 & substr(f, 5,5) %in% 1:4)) stop("Incorrect codes. See the output of `period_cd` for the required format!")
  q <- as.numeric(substr(f, 5, 5))
  y <- as.numeric(substr(f, 1, 4))
  ifelse(q == 4, paste0(y + 1, 1), paste0(y, q + 1))
}
