#' @name misc_funcs
#' @title Miscellaneous functions
#'
#' @description Miscellaneous functions
#' @rdname misc_funcs
#' @details \bold{\code{same_len_3dots}} - Logical test for matching lengths in an \code{ellipsis (...)}
#' @param ... ...
same_len_3dots <- function(...){
  lens <- as.numeric(lapply(list(...), length))
  lens <- lens[!duplicated(lens)]
  length(lens) == 1
}

#' @rdname misc_funcs
#' @details \bold{\code{len_3dots}} - Length of each entry in an \code{ellipsis (...)}
len_3dots <- function(...){
  as.numeric(lapply(list(...), length))
}
