#' @name bys_funcs
#' @title \code{bysort} in \code{R}
#'
#' @description R-styled versions of \code{STATA}'s \code{bysort} command.
#'
#' @param ... Sort levels. Passed to \code{order()}
#' @param by Groups
#' @param from_last Sort order. Passed to \code{order()}
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
#' cat <- c(1,1,5,3,1)
#' val <- c("K","Z","A","F","K")
#'
#' bys_rank(cat, by = cat)
#' bys_rank(val, by = cat)

#' @rdname bys_funcs
#' @export
bys_rank <- function(..., by, from_last= F){
  err <- err_bys_rank_1(..., by = by)
  if(err!=F) stop(err, call. = F)

  err <- err_bys_rank_2(..., by = by)
  if(err!=F) stop(err, call. = F)

  max_len <- max(as.numeric(lapply(list(..., by), length)))

  if(!same_len_3dots(...)) stop("Lengths of sort vectors (...) differ!")
  ell <- len_3dots(...)
  if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (...) and group (`by`) vectors differ!")
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")

  by <- match(by, by[!duplicated(by)])
  names(by) <- format(seq_len(length(by)) , trim = T, scientific = F)
  by2 <- by[order(by, ..., decreasing= from_last)]
  r <- sequence(rle(by2)$lengths)
  names(r) <- names(by2)
  o <- as.numeric(names(by2))
  names(o) <- r
  o <- sort(o)
  r <- as.numeric(names(o))
  return(r)
}

#' @rdname bys_funcs
#' @examples
#' bys_tot(by = cat)
#' @export
bys_tot <- function(by){
  if(!is.atomic(by)) stop("`by` must be an `atomic` vector!")
  if(length(by) == 0) stop("`by` has a length of 0!")

  by <- match(by, by[!duplicated(by)])
  names(by) <- format(seq_len(length(by)) , trim = T, scientific = F)
  by2 <- by[order(by)]
  r <- rep(rle(by2)$lengths, rle(by2)$lengths)
  names(r) <- names(by2)
  o <- as.numeric(names(by2))
  names(o) <- r
  o <- sort(o)
  r <- as.numeric(names(o))
  return(r)
}

#' @rdname bys_funcs
#' @param val Value
#'
#' @examples
#' bys_val(val, by = cat, val=val)
#' bys_val(val, by = cat, val=val, from_last = T)
#' @export
bys_val <- function(..., by, val, from_last= F){
  if(!same_len_3dots(...)) stop("Lengths of sort vectors (...) differ!")
  ell <- len_3dots(...)
  if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (...) and group (`by`) vectors differ!")
  if(ell[!duplicated(ell)] != length(val)) stop("Lengths of sort (...) and value (`val`) vectors differ!")
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")

  by <- match(by, by[!duplicated(by)])
  # rle's ordering is in reverse.
  # reverse every direction to accomodate for this
  from_last <- ifelse(from_last==T, F,T)
  names(val) <- names(by) <- format(seq_len(length(by)) , trim = T, scientific = F)
  by2 <- by[order(by, ..., decreasing= from_last, na.last=NA)]

  a <- rle(by2)
  r_val <- val[as.numeric(names(a$values))]

  #names(r_val) <- a$values
  r_val <- r_val[match(by, a$values)]
  names(r_val) <- NULL
  return(r_val)
}

#' @rdname bys_funcs
#' @param func Function
#'
#' @export
bys_func <- function(..., by, val, func, from_last= F){
  if(!same_len_3dots(...)) stop("Lengths of sort vectors (`...`) differ!")
  ell <- len_3dots(...)
  if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (`...`) and group (`by`) vectors differ!")
  if(ell[!duplicated(ell)] != length(val)) stop("Lengths of sort (`...`) and value (`val`) vectors differ!")
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  if(missing(func)) stop("Supply a function (`func`)!")

  by <- match(by, by[!duplicated(by)])
  ord <- bys_rank(..., by =by, from_last = from_last)

  vals <- lapply(split(val, by), func)
  vals <- vals[match(by, names(vals))]
  return(vals)
}
