library(lubridate) # for decimaldate

add_events_since <- function(df_incl_events, df_valid_index_events) {
  # add information on first occurrance of ascvd/aaa/pad/cevd/cad p patient
  # and binary indicators for each disease status on prediction moment
  get_first_event <- function(cat) {
    df_incl_events[category == cat, .(f = min(opndat)), keyby = RINPERSOON] |> 
      setnames(old = "f", new = paste0("first_", cat))
  }
  
  diseases <- c("cad", "pad", "aaa", "cevd")
  
  df_first_events <- Reduce(
    f = function(...) merge(..., all = T),
    x = lapply(diseases, get_first_event)
  ) 
  df_first_events[
    , first_ascvd := pmin(first_cad, first_pad, first_aaa, first_cevd, na.rm = T)
  ]
  
  df_valid_index_events <- df_first_events[df_valid_index_events]
  
  df_valid_index_events[
      , c(paste0("has_", diseases)) := lapply(.SD, function(x) (x <= T_prediction_moment) %in% T)
      , .SDcols = c(paste0("first_", diseases))
    ]
  
  df_valid_index_events[, years_since_first_ascvd := 
                          decimal_date(T_prediction_moment) - 
                          decimal_date(first_ascvd)]
  
  
  return(df_valid_index_events)
}

make_table_of_inclusion_events <- function(df_incl_events, df_outcome_events) {
  # make table of inclusion events that we are going to make predictions for.
  # person must be three months event free. Events close to each other are
  # considered the same. inclusion events with death soon thereafter are removed
  # because there is no prediction to be made. outcome events close to inclusion
  # are ignored - but will delay T0 (as patient needs 3 months event free)
  
  df_incl_events <- unique(
    df_incl_events[, .(RINPERSOON, event_date = opndat, type = "inclusie")]
  )
  df_all_events <- rbind(df_incl_events, df_outcome_events)
  
  setorderv(df_all_events, cols = c("RINPERSOON", "event_date"))
  
  df_all_events[
    , delta_days := as.numeric(
      difftime(
        time1 = event_date, 
        time2 = shift(event_date, fill = event_date[1]), 
        units = "days"
      )
    )
    , by = RINPERSOON]
  
  # group close events together
  df_all_events[
    , eventID := cumsum(delta_days >= max_event_duration) + 1L
    , by = RINPERSOON]
  
  # now find inclusion event groups: 
  # - need at least 1 inclusion event (first one defines start of event)
  # - dont die within group
  # - last event (incl/outcome) within group defines end of event
  
  df_all_events[
    , incl_event_group := any(type ==  "inclusie") & any(grepl("death", type)) == F
    , by = .(RINPERSOON, eventID)
  ]
  
  df_incl_intervals <- df_all_events[incl_event_group == T
                                     , .(event_start = min(event_date),
                                         event_end = max(event_date))
                                     , by = .(RINPERSOON, eventID)]
  
  return(df_incl_intervals[, .(RINPERSOON, incl_event_date = event_end)])
}

add_pat_info <- function(df_valid_index_events, df_pat) {
  df_valid_index_events <- df_pat[df_valid_index_events, on = "RINPERSOON"]
  df_valid_index_events[, T_prediction_moment := incl_event_date + max_event_duration]
  df_valid_index_events[, age := floor(decimal_date(T_prediction_moment) - decimal_date(dob))]
  
  return(df_valid_index_events)
}

exclude_patients <- function(df_valid_index_events) {
  df_valid_index_events <- df_valid_index_events[
    dInschrijfdatum <= incl_event_date - 180 & 
      incl_event_date <= dUitschrijfdatum & 
      incl_event_date >= t_begin_enrol &
      incl_event_date <= t_end_enrol, ]
  print(paste0(uniqueN(df_valid_index_events$RINPERSOON), " patients were registered in ELAN GP"))
  df_valid_index_events <- df_valid_index_events[
    age >= 40 & age <= 80, ]
  print(paste0(uniqueN(df_valid_index_events$RINPERSOON), " patients were between 40-80"))
  df_valid_index_events <- df_valid_index_events[!(Systeem == 3 | Systeem == 6), ]
  print(paste0(uniqueN(df_valid_index_events$RINPERSOON), " were in GP with compatible EHR system"))
  df_valid_index_events[order(incl_event_date), .SD[1], by = RINPERSOON]
}

add_outcome_info <- function(df_valid_index_events, df_outcome_events) {
  # add information on outcome
  df_valid_index_events[, c("event_date", "type") := 
                   (
                     df_outcome_events
                     [order(event_date)]
                     [df_valid_index_events
                       , on = .(RINPERSOON = RINPERSOON, event_date > incl_event_date)
                       , .(x.event_date, x.type)
                       , mult = "first"]
                   )
  ]
  
  df_valid_index_events[is.na(event_date), `:=`(
    event_date = t_end_fu,
    type = "censored"
  )]
  
  df_valid_index_events[, status := fcase(
    type == "death_cardial" | type == "uitkomst_hospital", "event",
    type == "death_other", "competing_event",
    type == "censored", "censored"
  )]
  
  df_valid_index_events[, type := NULL]
  
  df_valid_index_events[, time := decimal_date(event_date) - decimal_date(T_prediction_moment)]
  return(df_valid_index_events[])
}
