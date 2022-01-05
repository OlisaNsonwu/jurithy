#' @name ci
#' @title Confidence intervals for a difference between two proportions
#'
#' @param t1 Proportion 1
#' @param r1 Incidence rate 1
#' @param n1 Sample size 1
#' @param t2 Proportion 2
#' @param r2 Incidence rate 2
#' @param n2 Sample size 2
#' @param sig_level Significance level
#'
#' @references
#' https://www.bmj.com/content/343/bmj.d2304
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

ci_prop_diff <- function(t1, n1, t2, n2, sig_level = .95){
  if(!is.atomic(t1)) stop("`t1` must be an `atomic` vector!")
  if(!is.atomic(n1)) stop("`n1` must be an `atomic` vector!")
  if(!is.atomic(t2)) stop("`t2` must be an `atomic` vector!")
  if(!is.atomic(n2)) stop("`n2` must be an `atomic` vector!")
  if(!is.atomic(sig_level)) stop("`sig_level` must be an `atomic` vector!")
  if(!sig_level >= 0 && sig_level<= 1) stop("`sig_level` must be beyween 0 and  1!")

  p1 <- n1/t1
  p2 <- n2/t2
  se <- base::sqrt(((p1 * (1-p1))/n1) + ((p2 * (1-p2))/n2))
  alpha <- 0.5 * (1 - sig_level)
  z <- stats::qnorm(1 - alpha)

  diff <- p1 - p2
  z2 <- base::abs(diff/se)
  p <- base::exp(-0.717 * z2 - 0.416 * z2^2)
  list(diff = diff,
       se = se,
       p_value = p,
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

  se <- base::sqrt((1/n1)	+ (1/n2))
  alpha <- 0.5 * (1 - sig_level)
  z <- stats::qnorm(1 - alpha)

  rr <- r1/r2
  z2 <- base::abs(rr/se)
  p <- base::exp(-0.717 * z2 - 0.416 * z2^2)
  list(rate_ratio = rr,
       se = se,
       p_value = p,
       lo = base::exp(base::log(rr) - (z * se)),
       hi = base::exp(base::log(rr) + (z * se)))
}

#' @rdname ci
#' @examples
#' ci_rate_ratio(4.1, 157, 4.4, 178)
#'
#' @export
#'
#'

ci_risk_ratio <- function(n1, t1, n2, t2, sig_level = .95){
  if(!is.atomic(t1)) stop("`t1` must be an `atomic` vector!")
  if(!is.atomic(n1)) stop("`n1` must be an `atomic` vector!")
  if(!is.atomic(t2)) stop("`t2` must be an `atomic` vector!")
  if(!is.atomic(n2)) stop("`n2` must be an `atomic` vector!")
  if(!is.atomic(sig_level)) stop("`sig_level` must be an `atomic` vector!")
  if(!sig_level >= 0 && sig_level<= 1) stop("`sig_level` must be beyween 0 and  1!")

  alpha <- 0.5 * (1 - sig_level)
  z <- stats::qnorm(1 - alpha)

  rr <- (n1/t1)/(n2/t2)
  log_rr <- base::log(rr)
  se <- base::sqrt(((1/(n1))-(1/(t1))) + ((1/(n2))-(1/(t2))))
  z2 <- base::abs(rr/se)
  p <- base::exp(-0.717 * z2 - 0.416 * z2^2)
  lci <- base::exp(log_rr - (z * se))
  uci <- base::exp(log_rr + (z * se))
  stat_sig <- !(lci < 1 & uci > 1)
  list(risk_ratio = rr,
       se = se,
       p_value = p,
       lo = lci,
       hi = uci)

}
