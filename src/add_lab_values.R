source("H:/Jasper/datav2/utilities.R")

add_lab_values <- function(df_studypop, use_haga_lab) {
    
  wcia_codes <- fread("config/codes_wcia.csv", colClasses = "character")
  
  measurement_bounds <- fread("config/measurements_limits.csv", 
                              colClasses = list(numeric = c("lb", "ub")))
  
  df_lab <- open_dataset(FP_ELAN_LAB) |> 
    rename(RINPERSOON = RINPersoon) |> 
    filter(
      RINPERSOON %in% df_studypop$RINPERSOON,
      dWCIANummer %in% wcia_codes$wcia_code
    ) |> 
    select(RINPERSOON, dBepalingdatum, dMutatiedatum, dResultaatdatum,
           dWCIANummer, 
           Resultaat, Resultaattoevoeging, result = dResultaatnummer, Eenheid) |> 
    collect() |> 
    setDT()
  
  df_lab[, `:=`(
    dResultaatdatum = fcoalesce(dResultaatdatum, dMutatiedatum, dBepalingdatum),
    dMutatiedatum = NULL,
    dBepalingdatum = NULL
  )]
  
  df_lab <- wcia_codes[df_lab, on = .(wcia_code = dWCIANummer)]
  df_lab <- measurement_bounds[df_lab, on = .(meting)]
  
  df_lab[, Resultaat := gsub(",", ".", Resultaat)]
  
  df_lab[is.na(result), result := as.numeric(Resultaat)]
  df_lab <- df_lab[lb <= result & result <= ub, ]
  
  df_elanlab <- df_lab[, .(RINPERSOON, meting, date = dResultaatdatum, result = result)]
  
  # haga lab ----------------------------------------------------------------
  
  if (use_haga_lab == T) {
    relevant_measurements <- c("Choloesterol", "HDL Cholesterol", "eGFR (MDRD)",
                               "CRP", "geschatte GFR (CKD-EPI)", "Cholesterol", "sysbp")
    
    haga_lab_names <- fread("config/haga_lab_names.csv")
    
    df_haga_lab <- open_dataset(FP_HAGA_LAB) |> 
      filter(RINPERSOON %in% df_studypop$RINPERSOON,
             measurement %in% relevant_measurements
      ) |> 
      collect() |> 
      setDT()
    
    df_haga_lab[haga_lab_names, measurement := meting, on = .(measurement = haga_lab_name)]
    df_haga_lab <- measurement_bounds[df_haga_lab, on = .(meting = measurement)]
    df_haga_lab <- df_haga_lab[(result >= lb & result <= ub), ]
    df_haga_lab[meting == "crp", result := result/10]
    
    df_hagalab <- df_haga_lab[, .(RINPERSOON, meting, date, result)]
  
    df_lab <- rbindlist(list(df_elanlab, df_hagalab))
  } else {
    df_lab <- df_elanlab
  }
  
  
  # get nearest lab value ---------------------------------------------------
  
  df_studypop[, lab_window_start := incl_event_date]
  df_studypop[, lab_window_end := pmin(T_prediction_moment + 365, event_date)]
  
  df_studypop[, lab_window_start_prev := incl_event_date - 30 - 365]
  df_studypop[, lab_window_end_prev := incl_event_date - 30]
  
  df_studypop[, lab_window_start_next := pmin(lab_window_end, event_date)]
  df_studypop[, lab_window_end_next := pmin(lab_window_end + 365, event_date)]
  
  delta_days <- function(date1, date2) {
    as.numeric(abs(difftime(date1, date2, units = "days")))
  }
  
  get_nearest_lab_value <- function(meting_type, window_start, window_end) {
    # nearest lab value to prediction moment
    (
      df_studypop[df_lab[meting == meting_type]
                , on = c("RINPERSOON == RINPERSOON", 
                         paste0(window_start, "<=", "date"),
                         paste0(window_end, ">", "date"))
                , .(RINPERSOON, T_prediction_moment, i.date, result)
                , nomatch = NULL]
      [, .SD[which.min(delta_days(T_prediction_moment, i.date)), "result"], by = "RINPERSOON"] |> 
        setnames(old = "result", new = meting_type)
    )
  }
  
  get_table_of_lab_values <- function(window_start, window_end) {
    closest_lab_values <- lapply(
      X = as.list(unique(wcia_codes$meting)),
      FUN = function(x) get_nearest_lab_value(x, window_start, window_end)
    )
    all_lab_values <- Reduce(
      f = function(x,y) merge(x, y, all = T), 
      x = closest_lab_values
    )
    return(all_lab_values)
  }
  
  df_all_lab_values <- get_table_of_lab_values("lab_window_start", "lab_window_end")
  df_all_lab_values_prev <- get_table_of_lab_values("lab_window_start_prev", "lab_window_end_prev")
  df_all_lab_values_next <- get_table_of_lab_values("lab_window_start_next", "lab_window_end_next")
  
  clean_lab_further <- function(df_lab, var_name_append = "") {
    
    bound <- function(meetwaarde, bound) {
      measurement_bounds[meting == meetwaarde, get(bound)]
    }
    
    convert <- function(x, lower_bound, upper_bound) {
      fifelse(
        x < lower_bound | 
          x > upper_bound, 
        yes = NA_real_,
        no = x
      )
    }
    
    df_lab[is.na(nonhdl), nonhdl := convert(
      cholesterol - hdl, 
      lower_bound = bound("nonhdl", "lb"), 
      upper_bound = bound("nonhdl", "ub"))
    ]
    df_lab[is.na(nonhdl), nonhdl := convert(
      cholesterol - cholesterol/cholhdlratio, 
      lower_bound = bound("nonhdl", "lb"), 
      upper_bound = bound("nonhdl", "ub"))
    ]
    
    creatinine_to_egfr <- function(creatinine, is_woman, age) {
      egfr <- 175*(creatinine*0.0113)^(-1.154)*(age^(-0.203))
      egfr <- egfr * (0.742^is_woman)
      return(egfr)
    }
    
    df_lab <- df_studypop[, .(RINPERSOON, age, sex)][df_lab, on = "RINPERSOON"]
    
    df_lab[is.na(gfr), gfr := convert(
      creatinine_to_egfr(creatinine, sex == "V", age),
      lower_bound = bound("gfr", "lb"),
      upper_bound = bound("gfr", "ub")
    )]
    
    df_lab[, `:=`(age = NULL, sex = NULL)]
    
    setnames(
      df_lab, 
      old = unique(wcia_codes$meting), 
      new = function(x) paste0(x, var_name_append)
    )
    
    return(df_lab)
  }
  
  df_all_lab_values <- clean_lab_further(df_all_lab_values)
  df_all_lab_values_prev <- clean_lab_further(df_all_lab_values_prev, "_prev")
  df_all_lab_values_next <- clean_lab_further(df_all_lab_values_next, "_next")
  
  df_studypop <- Reduce(
    f = function(x,y) merge(x, y, all = T, by = "RINPERSOON"), 
    x = list(df_studypop, df_all_lab_values, df_all_lab_values_prev, df_all_lab_values_next)
  )

}
