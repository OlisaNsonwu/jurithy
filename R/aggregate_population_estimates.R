#' @title Aggregate population estimates
#'
#' @description Aggregate population estimates
#'
#' @param estimate Numeric. Population estimate
#' @param period Integer. Numeric codes for the corresponding period. See \link{period_cd}
#' @param from Character. Current level. "cy", "fy", "cq", "fq", "cm", "fm"
#' @param to Character. Desired level. "cy", "fy", "cq", "fq", "cm", "fm"
#' @param strata Atomic. Population strata
#'
#' @return A data frame
#'
#' @examples
#' dfr <- data.frame(
#'   ccg_cd = "E92001",
#'   cyear = c(2021, 2011),
#'   mid_yr_est = c(1457775, 1457783))
#'
#'
#' aggregate_population_estimates(
#'   strata = dfr$ccg_cd,
#'   estimate = dfr$mid_yr_est,
#'   period = dfr$cyear,
#'   from = "cy",
#'   to = "cq"
#' )
#'
#' aggregate_population_estimates(
#'   strata = dfr$ccg_cd,
#'   estimate = dfr$mid_yr_est,
#'   period = dfr$cyear,
#'   from = "cy",
#'   to = "fq"
#' )
#'
#' aggregate_population_estimates(
#'   strata = dfr$ccg_cd,
#'   estimate = dfr$mid_yr_est,
#'   period = dfr$cyear,
#'   from = "cy",
#'   to = "cm"
#' )
#'
#' aggregate_population_estimates(
#'   strata = dfr$ccg_cd,
#'   estimate = dfr$mid_yr_est,
#'   period = dfr$cyear,
#'   from = "cy",
#'   to = "fm"
#' )
#' @export
aggregate_population_estimates <- function(estimate, period, strata, from, to){
  pop_repo <- list(
    estimate = estimate,strata = strata,period = period)

  days_in_period <- get(paste0("days_in_", from))
  pop_repo$days_in_period <- days_in_period(pop_repo$period)
  lowest.unit <- gsub(".$", "m", from)
  if(from %in% c("cy", "fy")) {
    pop_repo <- lapply(1:12, function(i){
      pop_repo$lowest.unit <-
        (pop_repo$period * 100) + i
      as.data.frame(pop_repo)
    })
  }else if(from %in% c("cq", "fq")) {
    pop_repo <- lapply(1:3, function(i){
      pop_repo$lowest.unit <-
        (as.integer(pop_repo$period/10) * 100) +
        (pop_repo$period %% 4) + ((i-1) * 3)
      as.data.frame(pop_repo)
    })
  }else if(from %in% c("cm", "fm")) {
    pop_repo$lowest.unit <- pop_repo$period
  }

  days_lowest.unit <-
    ifelse(grepl("^c", lowest.unit),
           days_in_cm, days_in_fm)

  pop_repo <- do.call("rbind", pop_repo)
  pop_repo$days_lowest.unit <- days_lowest.unit(pop_repo$lowest.unit)

  pop_repo$lowest.unit.estimate <-
    pop_repo$estimate/pop_repo$days_in_period * pop_repo$days_lowest.unit

  if(lowest.unit != to){
    tgt.lvl_func <- paste0(ifelse(grepl("^c", lowest.unit),"cm", "fm"), "_to_", to)
    tgt.lvl_func <- get(tgt.lvl_func)
  }else{
    tgt.lvl_func <- identity
  }

  pop_repo$tgt.lvl.period <- tgt.lvl_func(pop_repo$lowest.unit)

  pop_repo <- group_by(pop_repo, strata, tgt.lvl.period)
  pop_repo <- summarise(pop_repo, est = sum(lowest.unit.estimate))
  pop_repo <- ungroup(pop_repo)

  nms <- names(pop_repo)
  nms <- ifelse(nms == "tgt.lvl.period", to, nms)
  nms -> names(pop_repo)

  pop_repo
}


