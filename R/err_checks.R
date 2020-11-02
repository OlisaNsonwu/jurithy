#' @name err_checks
#' @title Check for errors
#'
#' @description Check for errors
#' @rdname err_checks
#' @param ... ...
err_bys_rank_1 <- function(..., by){
  typs <- as.numeric(lapply(list(..., by), is.atomic))
  if(min(typs) != 1){
    "`by` and each element in `...` must be an atomic vector!"
  }else{
    F
  }
}

#' @rdname err_checks
err_bys_rank_2 <- function(..., by){
  lens_1 <- as.numeric(lapply(list(..., by), length))
  lens_1 <- list(min(lens_1), max(lens_1))

  # lens_2 <- as.numeric(lapply(list(..., by), length))
  # lens_2 <- list(max(lens_2), max(lens_2))

  if(lens_1[[1]] != lens_1[[2]]){
    "Each element in `...` must have the same length!"
  }
  if(!lens_1[[1]] %in% c(1, length(by))){
    "Length of `by` must be 1 or the same as `...`!"
  }
    F
}


