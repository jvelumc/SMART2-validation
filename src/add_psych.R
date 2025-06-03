source("H:/Jasper/datav2/utilities.R")

add_psych <- function(df_events) {
  
  # this shouldnt be hardcoded
  all_icpcs <- c(
    "alcohol" = "P15",
    "medication_abuse" = "P18",
    "drug_abuse" = "P19",
    "dementia" = "P70",
    "schizo" = "P72",
    "psychosisAff" = "P73",
    "anxiety" = "P74",
    "somatization" = "P75",
    "depressive" = "P76",
    "suicide" = "P77",
    "phobia_compulsive" = "P79",
    "personality_disorder" = "P80",
    "psychosisNOS" = "P98",
    "psychological_disorders_other" = "P99"
  )
  
                 
  df_diseases <- data.table(disease = paste0("icpc_", names(all_icpcs)), ICPC = all_icpcs)
  
  med_list <- c("N05A", "N06A")
  
  icpc_regex <- paste(all_icpcs, collapse = "|")
  med_regex <- paste(med_list, collapse = "|")
  
  df_episodes <- open_dataset(FP_ELAN_EPS) |> 
    filter(
      RINPersoon %in% df_events$RINPERSOON,
      grepl(icpc_regex, dICPC)
    ) |> 
    select(RINPERSOON = RINPersoon, dBegindatum, ICPC = dICPC) |> 
    collect() |> 
    setDT()
  
  df_episodes[, ICPC := stringr::str_extract(ICPC, icpc_regex)]
  df_episodes[df_diseases, on = "ICPC", disease := i.disease]
  
  
  df_comorb_table <- 
    (
      df_events
      [, .(RINPERSOON, T_prediction_moment)]
      [df_episodes, on = .(RINPERSOON)]
      [dBegindatum <= T_prediction_moment]
    )
  
  
  df_comorb_table <- df_comorb_table |> 
    dcast(
      RINPERSOON ~ disease, 
      fun.aggregate = function(x) as.numeric(length(x) >= 1),
      value.var = "disease"
    )
  
  df_comorb_table <- merge(df_events[, .(RINPERSOON)], df_comorb_table, by = "RINPERSOON", all = T)
  setnafill(df_comorb_table, type = "const", fill = 0, cols = df_diseases$disease)
  
  df_med <- open_dataset(FP_ELAN_MED) |> 
    filter(
      RINPersoon %in% df_events$RINPERSOON,
      grepl(med_regex, dATC)
    ) |> 
    select(
       RINPERSOON = RINPersoon, dVoorschrijfdatum, dATC, dSterkte
    ) |> 
    collect() |> 
    setDT()
  
  # any prescriptions, ever
  df_med[df_events, T_prediction_moment := i.T_prediction_moment, on = "RINPERSOON"]
  df_med <- df_med[!(dATC == "N05AH04" & clean_number(dSterkte) <= 75)]


  df_med[, `:=`(
    lithium = grepl("^N05AN", dATC),
    antipsych = grepl("^N05A", dATC),
    antidep = grepl("^N06A", dATC),
    year = year(dVoorschrijfdatum)
  )]

  df_med_p_year <- df_med[dVoorschrijfdatum <= T_prediction_moment, .(
    on_lithium = sum(lithium) >= 1,
    on_antipsych = sum(antipsych) >= 2,
    on_antidep = sum(antidep) >= 2
  ), by = .(RINPERSOON, year)]

  df_med_ever <- df_med_p_year[, .(
    med_lithium = any(on_lithium),
    med_antipsych = any(on_antipsych),
    med_antidep = any(on_antidep)
  ), by = .(RINPERSOON)]


  df_comorb_table <- df_med_ever[df_comorb_table, on = .(RINPERSOON)]
  lapply(
    list("med_lithium", "med_antipsych", "med_antidep"),
    function(x) fill_na_with_value(df_comorb_table, x, F)
  )
  
  
  cols <- names(df_comorb_table)[-1]
  df_comorb_table[, (cols) := lapply(.SD, as.logical), .SDcols = cols]
  
  df_comorb_table[, psych_icpcs := any(
    icpc_schizo, icpc_psychosisAff, 
    icpc_anxiety, icpc_somatization, icpc_depressive, icpc_phobia_compulsive, icpc_psychosisNOS,
    med_antipsych, med_lithium, med_antidep
  ), by = RINPERSOON]
  
  df_comorb_table[, epa_icpcs := any(icpc_schizo, 
                                  icpc_psychosisAff,
                                  icpc_psychosisNOS,
                                  med_antipsych,
                                  med_lithium), by = RINPERSOON]
  
  df_events <- df_comorb_table[, .(RINPERSOON, icpc_schizo, icpc_psychosisAff, 
                      icpc_anxiety, icpc_somatization, icpc_depressive, icpc_phobia_compulsive, icpc_psychosisNOS,
                      med_antipsych, med_lithium, med_antidep)][df_events, on = .(RINPERSOON)]
  
  df_events[, psych := RINPERSOON %in% df_comorb_table[psych_icpcs == T, RINPERSOON]]
  df_events[, psych_epa := RINPERSOON %in% df_comorb_table[epa_icpcs == T, RINPERSOON]]
  
  return(df_events)
}
