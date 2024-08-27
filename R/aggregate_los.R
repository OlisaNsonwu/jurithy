#' @title Aggregate duration of record level events
#'
#' @description Aggregate duration of record level events
#'
#' @param start_date Date. Event start date
#' @param end_date Date. Event end date
#' @param start_date_point Character|Date. Start point to begin aggregation.
#' Acceptable options are "start_of_day", "start_of_month", "start_of_quarter"
#' or "start_of_year". Alternatively, a specific date can be provided.
#' @param level Character. Desired level. Passed to 'by' in \code{\link{seq}}.
#' Options include all accepted in \code{\link[=seq]{seq(..., by = XX)}}
#' @param strata Atomic. Population strata
#' @param silent Logical. Print (or not) current iteration of loop.
#' @param count_id Atomic. If provided the unique count of \code{count_id}
#' per iteration (batch) is included
#' @param event_id Atomic. If provided the unique count of \code{event_id}
#' per iteration (batch) is included
#' @param fill Logical. If TRUE, place holders are included if
#' there is no occurrence of the event within the batch of the loop.
#' @param units Character. Desired unit for level. Passed to 'by' in \code{\link{seq}}.
#' Options include all accepted in \code{\link[=seq]{seq(..., by = XX)}}
#'
#' @return A data frame
#'
#' @examples
#' dfr <- data.frame(
#'   start_date = as.Date(c('2024-05-28', '2024-05-05', '2024-06-01')),
#'   end_date = as.Date(c('2024-06-05', '2024-05-11', '2024-06-20')),
#'   strata = c('A', 'A', 'B'))
#'
#' dfr$duration <- difftime(dfr$end_date, dfr$start_date, 'days')
#'
#' dfr
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date
#' )
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date,
#'   level = 'month'
#' )
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date,
#'   level = 'month',
#'   strata = dfr$strata
#' )
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date,
#'   level = 'month',
#'   strata = dfr$strata,
#'   start_date_point = 'start_of_month'
#' )
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date,
#'   level = 'month',
#'   strata = dfr$strata,
#'   start_date_point = 'start_of_month',
#'   units = 'weeks'
#' )
#'
#' aggregate_los(
#'   start_date = as.POSIXct(dfr$start_date),
#'   end_date = as.POSIXct(dfr$end_date),
#'   level = 'month',
#'   strata = dfr$strata,
#'   start_date_point = 'start_of_month',
#'   units = 'mins'
#' )
#'
#' aggregate_los(
#'   start_date = dfr$start_date,
#'   end_date = dfr$end_date,
#'   level = 'day',
#'   strata = dfr$strata,
#'   start_date_point = 'start_of_month',
#'   units = 'days'
#' )
#'
#' @export
aggregate_los <- function(
    start_date, end_date, level = diff(range(start_date, end_date)),
    strata = NULL, count_id = NULL, event_id = NULL, start_date_point = 'start_of_day',
    fill = FALSE, silent = TRUE, units = 'days'){

  dfr <- list(
    start_date = start_date,
    end_date = end_date,
    count_id = count_id,
    event_id = event_id,
    strata = strata
  )

  if((!inherits(dfr$start_date, c("POSIXct", "POSIXt")) |
     !inherits(dfr$end_date, c("POSIXct", "POSIXt")) |
     inherits(start_date_point, c("Date"))) & !units %in% c("days", "weeks")){
    stop("`start_date`, `end_date` and `start_date_point` must be `POSIXct` or `POSIXt` objects if `units` is not 'days' or 'weeks'")
  }

  if((inherits(dfr$start_date, c("POSIXct", "POSIXt")) |
      inherits(dfr$end_date, c("POSIXct", "POSIXt")) |
      inherits(start_date_point, c("POSIXct", "POSIXt"))) & units %in% c("days", "weeks")){
    stop("`start_date`, `end_date` and `start_date_point` must be `Date` objects if `units` is not 'days' or 'weeks'")
  }

  dfr$los_period <- difftime(dfr$start_date, dfr$end_date, units = units)

  ll_dt_a <- min(dfr$start_date)
  ll_dt_z <- max(dfr$end_date)

  if(inherits(start_date_point, c("POSIXct", "POSIXt", "Date"))){
    ll_dt_a <- start_date_point
  }else if(start_date_point == 'start_of_day'){

  }else if(start_date_point == 'start_of_quarter'){
    ll_dt_a <- as.Date(paste0(
      substr(period_cd(ll_dt_a, 'cq'), 1, 4), '-',
      stringr::str_pad(seq(1,12, by = 3)[
        as.numeric(substr(period_cd(ll_dt_a, 'cq'), 5, 5))
      ],  width=2, pad="0"), '-01'))
  }else if(start_date_point == 'start_of_year'){
    ll_dt_a <- as.Date(paste0(period_cd(ll_dt_a, 'cy'), '-01-01'))
  }else if(start_date_point == 'start_of_month'){
    ll_dt_a <- as.Date(paste0(
      substr(period_cd(ll_dt_a, 'cm'), 1, 4), '-',
      substr(period_cd(ll_dt_a, 'cm'), 5, 6), '-01'))
  }
  if(!units %in% c("days", "weeks")){
    ll_dt_a <- as.POSIXct(ll_dt_a)
  }

  bd_a <- seq(from = ll_dt_a, to = ll_dt_z, by = level)
  bd_a <- unique(c(bd_a, ll_dt_z))
  bd_z <- bys_lead(bd_a)

  if(
    (grepl('^day|^1 day|^1$', as.character(level)) & any(grepl('^day', units))) |
    (grepl('^sec|^1 sec|^1$', as.character(level)) & any(grepl('^sec', units)))
    ){
    indx <- 1:length(bd_a)-1
    bd_z[indx] <- bd_z[indx]-1

    indx <- length(bd_a)
    bd_z[indx] <- bd_a[indx]

  }else{
    indx <- length(bd_a)-1
    bd_a <- bd_a[1:indx]
    bd_z <- bd_z[1:indx]

    indx <- 1:length(bd_a)-1
    bd_z[indx] <- bd_z[indx]-1
  }


  bd_tot <- length(bd_a)
  los_lst <- list()
  for(i in seq_len(bd_tot)){
    if(!silent){
      print(paste0('Batch ', i, " of ", bd_tot))
    }

    dfr4 <- dfr
    lgk <- which(
      !(bd_z[i] < dfr4$start_date | bd_a[i] > dfr4$end_date)
    )
    dfr4 <- lapply(dfr4, function(x) x[lgk])

    if(length(dfr4[[1]]) == 0){
      if(fill){
        los_lst[[i]] <- list(
          strata = NA, overnight = 0, days = 0,
          unique_event_id = 0, unique_count_id = 0,
          batch = i, start_date = bd_a[i],
          end_date = bd_z[i])
      }
      next
    }

    # flag for spells that cut across months
    lgk_l <- dfr4$start_date < bd_a[i]
    dfr4$start_date[lgk_l] <- bd_a[i]
    lgk_r <- dfr4$end_date > bd_z[i]
    dfr4$end_date[lgk_r] <- bd_z[i]
    dfr4$tmp.los_a <-
      difftime(dfr4$end_date, dfr4$start_date, units = units)

    if(any(grep('sec|day', units))){
      dfr4$tmp.los_b <- dfr4$tmp.los_a + 1
    }else{
      dfr4$tmp.los_b <- rep(NA, length(dfr4$tmp.los_a))
    }

    # Adding 1-day diff (overnight stay) that's lost due to split from a previous month
    dfr4$tmp.los_a[lgk_l] <- dfr4$tmp.los_a[lgk_l] + 1
    dfr4$unique.count_id <- !duplicated(dfr4$count_id)
    dfr4$unique.event_id <- !duplicated(dfr4$event_id)

    if(length(dfr4$strata) == 0){
      dfr4$strata <- dfr4$cmb <- rep(NA, length(dfr4$start_date))
    }else{
      dfr4$cmb <- combi(dfr4$strata)
    }

    dfr4$diff <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$tmp.los_a))
    dfr4$duration <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$tmp.los_b), na.rm = FALSE)
    dfr4$unique.event_id <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$unique.event_id), na.rm = TRUE)
    dfr4$unique.count_id <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$unique.count_id), na.rm = TRUE)

    lgk <- which(!duplicated(dfr4$cmb))
    dfr4 <- lapply(dfr4[c("strata", "diff", "duration",
                          "unique.count_id", 'unique.event_id')],
                   function (x) x[lgk])

    dfr4$batch <- rep(i, length(dfr4$strata))

    dfr4$start_date <- bd_a[i]
    dfr4$end_date <- bd_z[i]

    los_lst[[i]] <- as.data.frame(dfr4)
    rm(dfr4)
  }
  los_lst <- do.call("rbind", los_lst)

  return(los_lst)
}


