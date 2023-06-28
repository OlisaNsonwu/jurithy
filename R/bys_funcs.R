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
#' cat <- c(1,1,5,3,1)
#' val <- c("K","Z","A","F","K")
#'
#' bys_rank(cat, by = cat)
#' bys_rank(val, by = cat)

#' @rdname bys_funcs
#' @export
bys_rank <- function(..., by, from_last= F){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  # This is important. Don't change.
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    z_pos <- order(by, decreasing= from_last)
  }else{
    err <- err_bys_rank_1(..., by = by)
    if(err!=F) stop(err, call. = F)

    err <- err_bys_rank_2(..., by = by)
    if(err!=F) stop(err, call. = F)

    max_len <- max(as.numeric(lapply(list(..., by), length)))

    if(!same_len_3dots(...)) stop("Lengths of sort vectors (...) differ!")
    ell <- len_3dots(...)
    if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (...) and group (`by`) vectors differ!")
    z_pos <- order(by, ..., decreasing= from_last)
  }

  a_pos <- seq_len(length(by))
  by2 <- by[z_pos]
  seq_ord <- sequence(rle(by2)$lengths)
  val_r <- seq_ord[match(a_pos, z_pos)]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @examples
#' bys_tot(by = cat)
#' @export
bys_tot <- function(by){
  if(!is.atomic(by)) stop("`by` must be an `atomic` vector!")
  if(length(by) == 0) stop("`by` has a length of 0!")
  # This is important. Don't change.
  by <- match(by, by[!duplicated(by)])
  a_pos <- seq_len(length(by))
  z_pos <- order(by)

  by2 <- by[z_pos]
  seq_rle <- rle(by2)
  val_r <- seq_rle$lengths[match(by, seq_rle$values)]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @param val Value
#'
#' @examples
#' bys_val(val, by = cat, val=val)
#' bys_val(val, by = cat, val=val, from_last = TRUE)
#' @export
bys_val <- function(..., by, val, from_last= F){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  # This is important. Don't change.
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    z_pos <- order(by, decreasing= from_last, na.last=NA)
  }else{
    if(!same_len_3dots(...)) stop("Lengths of sort vectors (...) differ!")
    ell <- len_3dots(...)
    if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (...) and group (`by`) vectors differ!")
    if(ell[!duplicated(ell)] != length(val)) stop("Lengths of sort (...) and value (`val`) vectors differ!")
    z_pos <- order(by, ..., decreasing= from_last, na.last=NA)
  }

  by2 <- by[z_pos]
  val2 <- val[z_pos]
  val2 <- val2[!duplicated(by2)]
  by2 <- by2[!duplicated(by2)]
  val_r <- val2[match(by, by2)]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

bys_val_retired <- function(..., by, val, from_last= F){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  # This is important. Don't change.
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    z_pos <- order(by, decreasing= from_last, na.last=NA)
  }else{
    if(!same_len_3dots(...)) stop("Lengths of sort vectors (...) differ!")
    ell <- len_3dots(...)
    if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (...) and group (`by`) vectors differ!")
    if(ell[!duplicated(ell)] != length(val)) stop("Lengths of sort (...) and value (`val`) vectors differ!")
    z_pos <- order(by, ..., decreasing= from_last, na.last=NA)
  }

  a_pos <- seq_len(length(by))
  by2 <- by[z_pos]
  z_pos <- z_pos[!duplicated(by2)]
  by2 <- by2[!duplicated(by2)]

  val_r <- val[z_pos[match(by, by2)]]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @param func Function
#'
#' @export
bys_func <- function(..., by, val, func, from_last= F){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  if(missing(func)) stop("Supply a function (`func`)!")
  # This is important. Don't change.
  by <- match(by, by[!duplicated(by)])
  if(length(list(...)) == 0){
    z_pos <- order(by, decreasing= from_last)
  }else{
    if(!same_len_3dots(...)) stop("Lengths of sort vectors (`...`) differ!")
    ell <- len_3dots(...)
    if(ell[!duplicated(ell)] != length(by)) stop("Lengths of sort (`...`) and group (`by`) vectors differ!")
    if(ell[!duplicated(ell)] != length(val)) stop("Lengths of sort (`...`) and value (`val`) vectors differ!")
    ord <- bys_rank(..., by =by, from_last = from_last)
  }

  val_r <- lapply(split(val, by), func)
  val_r <- val_r[by]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @param n Target position
#' @export
bys_lead_val <- function(..., by, val, from_last= F, n = 1){
  ord <- bys_rank(..., by = by, from_last =  from_last)
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by, ord)

  by2 <- by[s_ord]
  val2 <- val[s_ord]
  ord2 <- ord[s_ord]

  new_pos <- seq_len(length(by)) + (n + 1)
  by2 <- by[ord2]
  val2 <- val[ord2]

  val_r <- val2[match(by, by2)]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @export
bys_nval <- function(..., by, val, from_last= F, n = 1){
  ord <- bys_rank(..., by = by, from_last =  from_last)
  nt <- which(ord == (n + 1))
  by2 <- by[nt]
  val2 <- val[nt]
  val_r <- val2[match(by, by2)]
  rm(list = ls()[ls() != "val_r"])
  return(val_r)
}

#' @rdname bys_funcs
#' @export
bys_sum <- function(by, val){
  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  val <- val[s_ord]

  rp <- rle(by)
  val <- cumsum(val)
  lgk <- !duplicated(by, fromLast = TRUE)
  val <- val[lgk]
  by <- by[lgk]

  if(length(val) == 1){
    val <- rep(val, rp$lengths)
    return(val)
  }
  lag_pos <- 1:(length(val)-1)
  val <- val - c(0, val[lag_pos])
  val <- rep(val, rp$lengths)
  val <- val[order(s_ord)]
  return(val)
}
