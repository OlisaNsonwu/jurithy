#' @name bys_funcs
#' @title \code{bysort} in \code{R}
#'
#' @description R-styled versions of \code{STATA}'s \code{bysort} command.
#'
#' @param ... Sort levels. Passed to \code{order()}
#' @param by Groups
#' @param from_last Sort order. Passed to \code{order()}
#'
#' @return Atomic vector
#' @details
#' \bold{\code{bys_rank}} - Sort order of each record in a group
#' \itemize{
#' \item \bold{\code{STATA}} - \bold{\code{bysort group_var (sort_vars) : gen x = _n }}
#' \item \bold{\code{R}} - \bold{\code{bys_rank(sort_vars, by = group_var)}}
#' Use \code{from_last} to rank from in ascending (\code{FALSE}) or descending order (\code{TRUE})
#' }
#'
#' \bold{\code{bys_tot}} - Number of observations in a group of records
#' \itemize{
#' \item \bold{\code{STATA}} - \bold{\code{bysort group_var : gen x = _N }}
#' \item \bold{\code{R}} - \bold{\code{bys_tot(by = group_var)}}
#' }
#'
#' \bold{\code{bys_val}} - First or last observation of \bold{\code{`val`}} within a group of records
#'
#' \itemize{
#' \item \bold{\code{STATA}} - \bold{\code{bysort group_var (sort_vars) : gen val_var = val_var[1]}}
#' \item \bold{\code{R}} - \bold{\code{bys_val(sort_vars, by = group_var, val = val_var)}}
#'
#' \item \bold{\code{STATA}} - \bold{\code{bysort group_var (sort_vars) : gen val_var = val_var[_N]}}
#' \item \bold{\code{R}} - \bold{\code{bys_val(sort_vars, by = group_var, val = val_var, from_last = TRUE)}}
#' }
#'
#' \bold{\code{bys_func}} - Apply a function to group of records.
#'
#' @aliases bys_funcs
#' @examples
#' x <- data.frame(
#'   group = c(2, 2, 1, 2, 1, 1, 1, 2, 1, 1),
#'   value = c(13, 14, 20, 9, 2, 1, 8, 18, 3, 17))

#' bys_count(x$group)
#' bys_position(x$value, by = x$group, from_last = TRUE)
#' bys_rank(by = x$group, val = x$value, from_last = TRUE)
#' bys_val(x$value, by = x$group, val = x$value, from_last = TRUE)
#' bys_nval(x$value, by = x$group, val = x$value, from_last = TRUE, n = 2)
#' bys_min(by = x$group, val = x$value)
#' bys_max(by = x$group, val = x$value)
#' bys_sum(by = x$group, val = x$value)
#' bys_prod(by = x$group, val = x$value)
#' bys_cummin(by = x$group, val = x$value)
#' bys_cummax(by = x$group, val = x$value)
#' bys_cumsum(by = x$group, val = x$value)
#' bys_cumprod(by = x$group, val = x$value)

#' @rdname bys_funcs
#' @examples
#' bys_count(by = cat)
#' @export
bys_count <- function(by){
  if(!is.atomic(by)) stop("`by` must be an `atomic` vector!")
  if(length(by) == 0) stop("`by` has a length of 0!")

  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  rp <- rle(by)
  x <- rep(rp$lengths, rp$lengths)[order(s_ord)]
  return(x)
}

#' @rdname bys_funcs
#' @export
bys_rank <- function(..., by, from_last = FALSE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    s_ord <- order(by, decreasing = from_last, na.last = TRUE)
  }else{
    s_ord <- order(by, ..., decreasing = from_last, na.last = TRUE)
  }

  by <- by[s_ord]
  rp <- rle(by)
  x <- sequence(rp$lengths)[order(s_ord)]
  return(x)
}

#' @rdname bys_funcs
#' @export
bys_position <- function(val, by, from_last = FALSE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by, val, decreasing = from_last, na.last = TRUE)

  by <- by[s_ord]
  rp <- rle(by)
  x <- sequence(rp$lengths)

  by <- diyar::combi(by, val[s_ord])
  rp <- rle(by)
  faC <- as.integer(log10(max(rp$lengths))) + 1L
  faC <- 10 ^ faC
  x <- rep(x[!duplicated(by)] + (rp$lengths)/faC, rp$lengths)
  x <- x[order(s_ord)]
  return(x)
}

#' @rdname bys_funcs
#' @export
bys_tot <- bys_count

#' @rdname bys_funcs
#' @param val Value
#'
#' @examples
#' bys_val(val, by = cat, val=val)
#' bys_val(val, by = cat, val=val, from_last = TRUE)
#' @export
bys_val <- function(..., by, val, from_last = FALSE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    s_ord <- order(by, decreasing = from_last, na.last = TRUE)
  }else{
    s_ord <- order(by, ..., decreasing = from_last, na.last = TRUE)
  }

  by <- by[s_ord]
  val <- val[s_ord]
  rp <- rle(by)
  val <- rep(val[!duplicated(by)], rp$lengths)[order(s_ord)]
  return(val)
}

#' @rdname bys_funcs
#' @export
bys_nval <- function(..., by, val, from_last = FALSE, n = 1, et = TRUE){
  ord <- bys_position(..., by = by, from_last = from_last)
  s_ord <- !(ord >= n)

  if(et){
    val[s_ord] <- NA
  }

  return(
    bys_val(
      s_ord, ord,
      by = by, from_last = FALSE, val = val)
  )
}

#' @rdname bys_funcs
#' @export
bys_min <- function(by, val, na.rm = TRUE){
  val2 <- bys_val(val, by = by, from_last = FALSE, val = val)
  if(!na.rm){
    val2[
      by %in% by[is.na(val)]
    ] <- NA
  }
  return(val2)
}

#' @rdname bys_funcs
#' @export
bys_max <- function(by, val, na.rm = TRUE){
  val2 <- bys_val(val, by = by, from_last = TRUE, val = val)
  if(!na.rm){
    val2[
      by %in% by[is.na(val)]
    ] <- NA
  }
  return(val2)
}

#' @rdname bys_funcs
#' @export
bys_sum <- function(by, val, na.rm = TRUE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  if(na.rm){
    val[is.na(val)] <- 0
  }

  rp <- rle(by)
  cum.val <- cumsum(val)
  lgk <- !duplicated(by, fromLast = TRUE)
  max.val <- cum.val[lgk]
  by <- by[lgk]

  if(length(max.val) == 1){
    max.val <- rep(max.val, rp$lengths)
    return(max.val)
  }

  lag_pos <- 1:(length(max.val) - 1)
  prv_max.val <- c(0, max.val[lag_pos])
  max.val <- max.val - c(0, max.val[lag_pos])
  max.val <- rep(max.val, rp$lengths)
  max.val <- max.val[order(s_ord)]
  return(max.val)
}

#' @rdname bys_funcs
#' @export
bys_prod <- function(by, val, na.rm = TRUE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  if(na.rm){
    val[is.na(val)] <- 1
  }

  rp <- rle(by)
  cum.val <- cumprod(val)
  lgk <- !duplicated(by, fromLast = TRUE)
  max.val <- cum.val[lgk]
  by <- by[lgk]

  if(length(max.val) == 1){
    max.val <- rep(max.val, rp$lengths)
    return(max.val)
  }

  lag_pos <- 1:(length(max.val) - 1)
  prv_max.val <- c(0, max.val[lag_pos])
  max.val <- max.val / c(1, max.val[lag_pos])
  max.val <- rep(max.val, rp$lengths)
  max.val <- max.val[order(s_ord)]
  return(max.val)
}

#' @rdname bys_funcs
#' @export
bys_cummin <- function(by, val, na.rm = TRUE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  indx <- which(!is.na(val))
  if(length(indx) == 0){
    return(rep(NA, length(val)))
  }
  RNG <- range(val[indx])
  if(RNG[[1]] == RNG[[2]]){
    return(rep(RNG[[1]], length(val)))
  }

  faC <- as.integer(log10(RNG[[2]] - RNG[[1]])) + 1L
  faC <- 10 ^ faC

  if(na.rm){
    val[is.na(val)] <- 0
  }

  by <- ((max(by) + 1) - by) * faC
  val <- -(by + val)
  val <- abs(cummax(val)) - by
  val <- val[order(s_ord)]
  return(val)
}

#' @rdname bys_funcs
#' @export
bys_cummax <- function(by, val, na.rm = FALSE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  indx <- which(!is.na(val))
  if(length(indx) == 0){
    return(rep(NA, length(val)))
  }
  RNG <- range(val[indx])
  if(RNG[[1]] == RNG[[2]]){
    return(rep(RNG[[1]], length(val)))
  }

  if(na.rm){
    val[is.na(val)] <- 0
  }

  faC <- as.integer(log10(RNG[[2]] - RNG[[1]])) + 1L
  faC <- 10 ^ faC


  by <- by * faC
  val <- by + val
  val <- cummax(val) - by
  val <- val[order(s_ord)]
  return(val)
}

#' @rdname bys_funcs
#' @export
bys_cumsum <- function(by, val, na.rm = TRUE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  if(na.rm){
    val[is.na(val)] <- 0
  }

  rp <- rle(by)
  cum.val <- cumsum(val)
  lgk <- !duplicated(by, fromLast = TRUE)
  max.val <- cum.val[lgk]
  by <- by[lgk]

  if(length(max.val) == 1){
    max.val <- rep(max.val, rp$lengths)
    return(max.val)
  }

  lag_pos <- 1:(length(max.val) - 1)
  prv_max.val <- c(0, max.val[lag_pos])
  cum.val <- cum.val - rep(prv_max.val, rp$lengths)
  cum.val <- cum.val[order(s_ord)]
  return(cum.val)
}

#' @rdname bys_funcs
#' @export
bys_cumprod <- function(by, val, na.rm = TRUE){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  if(na.rm){
    val[is.na(val)] <- 1
  }

  rp <- rle(by)
  cum.val <- cumprod(val)
  lgk <- !duplicated(by, fromLast = TRUE)
  max.val <- cum.val[lgk]
  by <- by[lgk]

  if(length(max.val) == 1){
    max.val <- rep(max.val, rp$lengths)
    return(max.val)
  }

  lag_pos <- 1:(length(max.val) - 1)
  prv_max.val <- c(1, max.val[lag_pos])
  cum.val <- cum.val / rep(prv_max.val, rp$lengths)
  cum.val <- cum.val[order(s_ord)]
  return(cum.val)
}




