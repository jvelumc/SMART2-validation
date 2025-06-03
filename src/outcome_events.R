get_outcome_events <- function(rin_list) {
  # gets a list of all outcome events
  # 
  # Note: this script and the one for reading in inclusion_events.R could
  # probably be refactored such that they reuse more of eachothers code.
  
  df_icd10_codes <- fread("config/codes_outcome_icd10.csv")
  df_icd9_codes <- fread("config/codes_outcome_icd9.csv")
  
  codes_to_regex <- function(Fatal, Exclude) {
    # format the ICD10 outcome codes to a regex string, necessary for reading in 
    # from LBZ files
    paste(
      paste0(
        "^", 
        df_icd10_codes[fatal == Fatal & exclude == Exclude, icd10]
      ), 
      collapse = "|"
    )
  }
  
  lbz_incl_codes <- codes_to_regex(0,0)
  lbz_excl_codes <- codes_to_regex(0,1)
  
  df_lbz <-
    open_dataset(FP_LBZ) |> 
    filter(
      RINPERSOON %in% rin_list,
      grepl(lbz_incl_codes, LBZIcd10hoofddiagnose) &
        !grepl(lbz_excl_codes, LBZIcd10hoofddiagnose)) |> 
    select(RINPERSOON, event_date = LBZOpnamedatum) |> 
    collect() |> 
    setDT()
  
  # lmr (2000-2012) uses mostly icd9, but some hospitals already switched 
  # to icd10 in 2010. 
  df_lmr <- 
    open_dataset(FP_LMR) |> 
    filter(
      RINPERSOON %in% rin_list,
      hfddiag %in% df_icd9_codes$icd9_nodot |
        (
          grepl(lbz_incl_codes, icd10hfddiag) &
            !grepl(lbz_excl_codes, icd10hfddiag)
        )
    ) |> 
    select(RINPERSOON, event_date = opndat) |> 
    collect() |> 
    setDT()
  
  hospital_events <- rbind(df_lmr, df_lbz)
  hospital_events[, type := "uitkomst_hospital"]
  
  # get events from deaths, i.e. cardial death (primary outcome) or other deaths
  # (competing outcome)
  df_sterfte <- 
    open_dataset(FP_STERFTE) |> 
    filter(RINPERSOON %in% rin_list) |> 
    rename(event_date = GBADatumOverlijden) |> 
    collect() |> 
    setDT()
  
  df_sterfte[, 
             type := fifelse(
               grepl(codes_to_regex(1,0), icd10_cause) &
                 !grepl(codes_to_regex(1,1), icd10_cause),
               "death_cardial",
               "death_other"
             )
  ]
  
  df_sterfte[, icd10_cause := NULL]
  df_outcome_events <- rbind(hospital_events, df_sterfte)
  setorderv(df_outcome_events, cols = c("RINPERSOON", "event_date"))
  
  # remove events after T_end_fu
  df_outcome_events <- df_outcome_events[event_date <= t_end_fu, ]
  
  return(df_outcome_events)
}
