#' @name ci
#' @title Confidence intervals for a difference between two proportions
#'
#' @param p1 Proportion 1
#' @param r1 Incidence rate 1
#' @param n1 Sample size 1
#' @param p2 Proportion 2
#' @param r2 Incidence rate 2
#' @param n2 Sample size 2
#'
#' @aliases ci
#' @return \code{numeric}
#'
#' @examples
#'
#' ci_prop_diff(0.56, 80, 0.76, 80)
#'
#'
#' @export

ci_prop_diff <- function(p1, n1, p2, n2, sig_level = .95){
  if(!is.atomic(p1)) stop("`p1` must be an `atomic` vector!")
  if(!is.atomic(n1)) stop("`n1` must be an `atomic` vector!")
  if(!is.atomic(p2)) stop("`p2` must be an `atomic` vector!")
  if(!is.atomic(n2)) stop("`n2` must be an `atomic` vector!")
  if(!is.atomic(sig_level)) stop("`sig_level` must be an `atomic` vector!")
  if(!sig_level >= 0 && sig_level<= 1) stop("`sig_level` must be beyween 0 and  1!")

  se <- sqrt(((p1 * (1-p1))/n1) + ((p2 * (1-p2))/n2))
  alpha <- 0.5 * (1 - sig_level)
  z <- qnorm(1 - alpha)

  list(diff = p2 - p1,
       se = se,
       lo = (p2 - p1) - (z * se),
       hi = (p2 - p1) + (z * se))
}

#' @rdname ci
#' @examples
#' ci_rate_ratio(4.1, 157, 4.4, 178)
#'
#' @export
#'
#'

ci_rate_ratio <- function(r1, n1, r2, n2, sig_level = .95){
  if(!is.atomic(r1)) stop("`r1` must be an `atomic` vector!")
  if(!is.atomic(n1)) stop("`n1` must be an `atomic` vector!")
  if(!is.atomic(r2)) stop("`r2` must be an `atomic` vector!")
  if(!is.atomic(n2)) stop("`n2` must be an `atomic` vector!")
  if(!is.atomic(sig_level)) stop("`sig_level` must be an `atomic` vector!")
  if(!sig_level >= 0 && sig_level<= 1) stop("`sig_level` must be beyween 0 and  1!")

  se <- sqrt((1/n1)	+ (1/n1))
  alpha <- 0.5 * (1 - sig_level)
  z <- qnorm(1 - alpha)

  rr <- r2/r1
  list(ratio = rr,
       se = se,
       lo = exp(log(rr) - (z * se)),
       hi = exp(log(rr) + (z * se)))
}
