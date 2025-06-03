library(arrow)
library(data.table)
library(dplyr)
library(stringr)

source("H:/Jasper/datav2/utilities.R")

add_medication <- function(df_events) {
  
  med_dict <- fread("config/codes_medication.csv", key = "atc")
  drug_regex <- paste0("^(", paste(med_dict$atc, collapse = "|"), ")")
  
  df_med <- open_dataset(FP_ELAN_MED) |> 
    rename(RINPERSOON = RINPersoon) |> 
    filter(
      RINPERSOONS == "R",
      RINPERSOON %in% df_events$RINPERSOON,
      grepl(drug_regex, dATC)
    ) |> 
    select(RINPERSOON, dVoorschrijfdatum, dATC, dHoeveelheid, Dosiscode, dSterkte) |> 
    collect() |> 
    setDT(key = "RINPERSOON")
  
  df_med[, atc_main := str_extract(dATC, drug_regex)]
  df_med <- med_dict[df_med, on = .(atc = atc_main)]
  
  # antithrombotica: needs to be prescribed after inclusion and before 1 year
  # after prediction
  df_events[, on_antithrombotic := RINPERSOON %in%
              (
                df_events
                [df_med[med_name == "antithrombotic"], on = .(RINPERSOON)]
                [dVoorschrijfdatum >= incl_event_date &
                    dVoorschrijfdatum <= pmin(T_prediction_moment + 365, event_date)]
                [, unique(RINPERSOON)]
              )
  ]
  
  return(df_events)
}
