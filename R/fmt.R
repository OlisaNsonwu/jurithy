#' @title Common figure formats
#'
#' @description Converts a number to favoured formats for inclusion in texts and reports.
#'
#'
#' @param x Numerical vector to be formatted.
#' @param fmt Character vector specifying the type of format to be applied.
#'
#' @seealso \code{\link{fy}}
#'
#' @return A Character vector
#'
#' @examples
#'
#' fmt(10.34456,"rate")
#'
#' fmt(10.34456,"rate", dp=3)
#'
#' fmt(.1,"rate", dp=3)
#'
#' paste0(fmt(10.34456,"percent"),"%")
#'
#' fmt(1034456,"count")
#'
#' fmt(.001,"rate")
#'
#' fmt(.001,"p_val")
#'
#' fmt(.001,"percent")
#'
#'
#' @export

fmt <- function(x, fmt = "count", dp = 1){
  if(!is.atomic(x) == 0) stop("`x` must be an `atomic` vector!")
  if(length(fmt) != 1) stop("`fmt` must have a length of 1!")
  if(fmt %in% c("count", "rate", "rate_abb",
                "multi_var", "percent", "fy_abb", "fy_full")) stop("`fmt` must be one of ",
                                                                   paste0(c("count", "rate", "rate_abb",
                                                                            "multi_var", "percent", "fy_abb", "fy_full"), collapse = ", "),
                                                                   "!")

  if(length(dp) != 1) stop("`dp` must have a length of 1!")
  d <- x

  if(fmt %in% c("count")){
    funx <- function(g){formatC(janitor::round_half_up(g), format="d", big.mark=",")}
  }else if (fmt %in% c("rate","rate_abb","multi_var","percent")){
    funx <- function(g){formatC(janitor::round_half_up(g,dp), digits = dp, format = "f")}
  }else if (fmt %in% c("p_val")){
    funx <- function(g){
      paste("= ",formatC(janitor::round_half_up(g,dp), digits = dp, format = "f"), sep="")
    }
  }else if(fmt == "fy_abb"){
    funx <- fy(g, fmt = "abb")
  }else if(fmt == "fy_full"){
    funx <- fy(g, fmt = "full")
  }

  z <- dplyr::case_when(
    is.na(d) ~ NA_character_,
    d==0 & !fmt %in% c("p_val","multi_var") ~ "0" ,
    janitor::round_half_up(d,dp) < 0.01 & fmt %in% c("p_val","multi_var")~ "< 0.01",
    janitor::round_half_up(d,dp) < 0.1 & fmt %in% c("rate","rate_abb") ~ "< 0.1",
    janitor::round_half_up(d,dp) < 1 & fmt == "percent" ~ "< 1",
    TRUE ~ funx(d)
  )

  return(z)
}
