#' @title Financial year labels as used in routine reports
#'
#' @description Favoured formats for labelling financial years.
#'
#'
#' @param x Numerical vector - Financial year. Accepts; 2018, 201819 or 20182019.
#' @param fmt Character vector - "abb" or "full". Short hand format e.g. "2018/19" or full description e.g. "April 2018 to March 2019".
#'
#' @seealso \code{\link{fmt}}
#'
#' @return Character - A description of the financial year.
#'
#' @examples
#'
#' fy(2018)
#'
#' fy(201819)
#'
#' fy(20182019)
#'
#' @export

fy <- function(x, fmt = "abb"){
  if(!is.atomic(x)) stop("`x` must be an `atomic` vector!")
  if(length(fmt) != 1) stop("`fmt` must have a length of 1!")
  if(fmt %in% c("abb", "full")) stop("`fmt` must be one of ",
                                     paste0(c("abb", "full"), collapse = ", "),"!")

  if (fmt == "abb"){
    x <- paste(
      substr(x,1,4),
      substr(as.numeric(substr(x,1,4))+1,3,4),
      sep="/"
    )
  } else if(fmt == "full"){
    x <- paste(
      "April",
      substr(x,1,4),
      "to March",
      as.numeric(substr(x,1,4))+1,
      sep=" "
    )
  }

  return(x)
}
