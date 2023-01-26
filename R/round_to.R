#' @title Round to
#'
#' @param val Ordered vector
#' @param by Increment
#' @param func Function
#'
#' @return Vector of he same class as \code{val}
#'
#' @examples
#'
#' round_to(1104.3, 5)
#' round_to(1104.3, 5, ceiling)
#' round_to(1104.3, 5, trunc)
#' @export

round_to <- function(x, to, f = round){
  r <- (x %% to)
  (x - r) + (f(r/to) * to)
}


