#' @name bys_funcs
#'
#' @title Vectorised approach to group operations.
#'
#' @param ... \code{[atomic]}. Sort levels
#' @param by \code{[atomic]}. Groups.
#' @param n \code{[integer]} Position.
#' @param nmax \code{[logical]} If \code{TRUE}, use \code{length([by])} when \code{n} is greater than the number of records in a group.
#' @param from_last \code{[logical]} Sort order - \code{TRUE} (descending) or \code{FALSE} (ascending).
#' @param val \code{[atomic]}. Value
#'
#' @return \code{[atomic]}
#'
#' @aliases bys_funcs
#' @examples
#' x <- data.frame(
#'   group = c(2, 2, 1, 2, 1, 1, 1, 2, 1, 1),
#'   value = c(13, 14, 20, 9, 2, 1, 8, 18, 3, 17))
#'
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
#' bys_lag(by = x$group, val = x$value)
#' bys_lead(by = x$group, val = x$value)

#' @rdname bys_funcs
#' @export
bys_count <- diyar::bys_count

#' @rdname bys_funcs
#' @export
bys_tot <- diyar::bys_count

#' @rdname bys_funcs
#' @export
bys_rank <- diyar::bys_rank

#' @rdname bys_funcs
#' @param ordered If \code{TRUE}, values are sequential.
#' @export
bys_position <- diyar::bys_position

#' @rdname bys_funcs
#' @export
bys_val <- diyar::bys_val

#' @rdname bys_funcs
#' @export
bys_nval <- diyar::bys_nval

#' @rdname bys_funcs
#' @param na.rm If \code{TRUE}, remove \code{NA} values
#' @export
bys_min <- diyar::bys_min

#' @rdname bys_funcs
#' @export
bys_max <- diyar::bys_max

#' @rdname bys_funcs
#' @export
bys_sum <- diyar::bys_sum

#' @rdname bys_funcs
#' @export
bys_prod <- diyar::bys_prod

#' @rdname bys_funcs
#' @export
bys_cummin <- diyar::bys_cummin

#' @rdname bys_funcs
#' @export
bys_cummax <- diyar::bys_cummax

#' @rdname bys_funcs
#' @export
bys_cumsum <- diyar::bys_cumsum

#' @rdname bys_funcs
#' @export
bys_cumprod <- diyar::bys_cumprod

#' @rdname bys_funcs
#' @export
bys_lag <- diyar::bys_lag

#' @rdname bys_funcs
#' @export
bys_lead <- diyar::bys_lead
