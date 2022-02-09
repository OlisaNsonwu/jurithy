#' @title Number of days in a period
#' @name period_days
#' @param x Time code. Output of \code{period_cd()}
#' @return \code{numeric}.
#' @aliases period_days
#' @examples
#' days_in_cm(202002)
#' days_in_fm(202002)
#' days_in_cq(20202)
#' days_in_fq(20202)
#' days_in_cy(2020)
#' days_in_fy(2020)
#'
#' @export
days_in_cm <- function(x){
  days <- rep(NA_real_, length(x))
  y <- as.numeric(substr(x, 1, 4))
  m <- as.numeric(substr(x, 5, 6))
  days[m %in% c(1, 3, 5, 7, 8, 10, 12)] <- 31L
  days[m == 2 & y %% 4 == 0] <- 29L
  days[m == 2 & y %% 4 != 0] <- 28L
  days[is.na(days)] <- 30L
  days
}

#' @rdname period_days
#' @export
days_in_fm <- function(x){
  days_in_cm(fm_to_cm(x))
}

#' @rdname period_days
#' @export
days_in_cq <- function(x){
  days <- rep(NA_real_, length(x))
  y <- as.numeric(substr(x, 1, 4))
  q <- as.numeric(substr(x, 5, 5))
  days[q %in% 3:4] <- 92L
  days[q == 2] <- 91L
  days[q == 1 & y %% 4 == 0] <- 91L
  days[q == 1 & y %% 4 != 0] <- 90L
  days
}

#' @rdname period_days
#' @export
days_in_fq <- function(x){
  days_in_cq(fq_to_cq(x))
}

#' @rdname period_days
#' @export
days_in_cy <- function(x){
  days <- rep(NA_real_, length(x))
  days[x %% 4 == 0] <- 366L
  days[x %% 4 != 0] <- 365L
  days
}

#' @rdname period_days
#' @export
days_in_fy <- function(x){
  y <- as.numeric(substr(x, 1, 4)) + 1
  days <- rep(NA_real_, length(x))
  days[y %% 4 == 0] <- 366L
  days[y %% 4 != 0] <- 365L
  days
}
