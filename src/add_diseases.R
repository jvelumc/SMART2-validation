library(data.table)
library(dplyr)
library(arrow)
library(stringr)

source("H:/Jasper/datav2/utilities.R")

add_diseases <- function(df_events) {
  
  icpc_dict_diseases <- fread("config/codes_diseases.csv", key = "ICPC")
  all_icpcs <- icpc_dict_diseases
  
  icpc_regex <- paste(all_icpcs$ICPC, collapse = "|")
  
  df_diseases <- open_dataset(FP_ELAN_EPS) |> 
    rename(RINPERSOON = RINPersoon) |> 
    filter(
      RINPERSOONS == "R", 
      RINPERSOON %in% df_events$RINPERSOON,
      grepl(icpc_regex, dICPC)
    ) |> 
    select(RINPERSOON, dICPC, dBegindatum) |> 
    collect() |> 
    setDT(key = "RINPERSOON")
  
  
  df_diseases[, ICPC := str_extract(dICPC, icpc_regex)]
  df_diseases <- all_icpcs[df_diseases, on = "ICPC"]
  
  df_comorb_table <- 
    (
      df_events
      [, .(RINPERSOON, T_prediction_moment)]
      [df_diseases, on = .(RINPERSOON)]
      [dBegindatum <= T_prediction_moment]
      [, .(
        diabetes = any(disease == "diabetes")
      ), by = RINPERSOON]
    )
  
  df_events <- df_comorb_table[df_events, on = .(RINPERSOON)]

  fill_na_with_value(df_events, "diabetes", F)
  
  return(df_events)
}

