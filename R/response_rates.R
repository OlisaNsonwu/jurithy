#' @title Response rates
#'
#' @description Response rate for every value in a \code{list} of \code{atomic} vectors
#'
#' @param x \code{list} of \code{atomic} vectors or \code{data.frame}
#'
#' @return \code{list}
#'
#' @examples
#' dataset <- data.frame(a = c("A", "C", "D"),
#'                       b = c("D", "A", "B"),
#'                       c = c("C", "E", "B"))
#'
#' dataset
#' response_rates(dataset)
#' cbind(dataset, as.data.frame(response_rates(dataset)))
#' @export

response_rates <- function(x){
  if(!any(class(x) %in% c("list", "data.frame") )) stop("`x` must be a `list` object!")
  if(any(!atomic_content(x))) stop("Every element in `...` must be an atomic vector!")
  vals <- as.list(x)
  err <- lapply(vals, function(x){
    any(nchar(x) != 1)
  })
  err <- unlist(err, use.names = FALSE)
  if(any(err)){
    err <- which(err)
    stop(paste0("Each value in every element/column must be have a character length of 1!\n",
                "i - This is not the case in the elements/columns below\n",
                paste0("X - ", names(x)[err], collapse = "\n")))
  }
  s <- paste0("vals[[", seq_len(length(vals)) ,"]]", collapse = ", ")
  v <- eval(parse(text = paste0("paste(",s,",sep='-')")))
  opts <- eval(parse(text = paste0("c(",s,")")))
  opts <- opts[!duplicated(opts)]
  tms <- function(c) nchar(gsub(paste0("[^",c,"]|-"),"",v))
  hits <- lapply(opts, tms)

  names(hits) <- opts
  hits[["response_total"]] <- rep(length(vals), length(vals[[1]]))
  hits[["response_vals"]] <- v
  hits <- hits[c(opts, "response_total", "response_vals")]
  return(hits)
}
