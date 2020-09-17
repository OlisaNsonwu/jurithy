#' @title Rolling function of a numeric values
#'
#' @param val Ordered vector
#' @param by Increment
#' @param func Function
#'
#' @return Vector of he same class as \code{val}
#'
#' @examples
#'
#' rolling_value(1:20, by = 5)
#' rolling_value(1:20, by = 10)
#' rolling_value(1:20, by = 10, func = mean)
#'
#' @export

rolling_value <- function(val, by = 12, func = sum){
  if(!is.atomic(val)) stop("`val` must be an `atomic` vector!")
  if(length(by) != 0) stop("`by` must have a length of 1!")
  if(!is.function(func)) stop("`func` must have a `function`!")

  by <- abs(by)
  p <- 1:length(val)

  roll_seq <- function(pos){
    if(pos >= by) {
      pos <- ((pos-(by-1)):pos)
    }else{
      pos <- (1:pos)
    }
    func(val[pos])
  }

  unlist(lapply(p, roll_seq))
}

