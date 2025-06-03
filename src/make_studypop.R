source("config/config.R")
source("src/inclusion_events.R")
source("src/outcome_events.R")
source("src/process_inclusion_and_outcomes.R")

source("src/add_bmi.R")
source("src/add_diseases.R")
source("src/add_herkomst.R")
source("src/add_lab_values.R")
source("src/add_medication.R")
source("src/add_psych.R")
source("src/add_ses.R")
source("src/add_smoking.R")

make_studypop <- function(use_haga) {
  
  # start with all ELAN patients
  df_pat <- open_dataset(FP_ELAN_PAT_CLEAN) |> 
    collect() |> 
    setDT()
  
  # Of these patients, extract all hospital events leading to possible inclusion
  df_incl_events <- get_all_incl_events(unique(df_pat$RINPERSOON))
  
  # extract all outcome events for patients that had an inclusion event
  df_outcome_events <- get_outcome_events(unique(df_incl_events$RINPERSOON))
  
  # select valid inclusion events to make predictions for: 
  #   - choose last event (incl/outcomes) of events close to each other
  #   - must be 3 months event free
  df_valid_index_events <- make_table_of_inclusion_events(df_incl_events, df_outcome_events)
  
  # add characteristics from ELAN pat table
  df_valid_index_events <- add_pat_info(df_valid_index_events, df_pat)
  
  # exclude patients, for not being in ELAN practice of time of event, 
  # not correct age, event not within T_begin_enrol or T_end_enrol,
  # also select only first time all inclusion criteria are met
  df_studypop <- exclude_patients(df_valid_index_events)
  
  # add information on first ever ASCVD events
  df_studypop <- add_events_since(df_incl_events, df_studypop)
  
  # add information on (primary / competing) outcomes to table
  df_studypop <- add_outcome_info(df_studypop, df_outcome_events)
  
  # add covariates and other variables
  df_studypop <- add_diseases(df_studypop)
  df_studypop <- add_smokers(df_studypop)
  df_studypop <- add_medication(df_studypop)
  df_studypop <- add_herkomst(df_studypop)
  df_studypop <- add_lab_values(df_studypop, use_haga)
  df_studypop <- add_ses(df_studypop)
  df_studypop <- add_bmi(df_studypop)
  df_studypop <- add_psych(df_studypop)
  
  # Further exclude patients in incompatible EHR system, or of GPs who send very 
  # little data 
  df_studypop[, no_lab := is.na(sbp) & is.na(nonhdl) & is.na(gfr) & is.na(crp) ]
  
  
  # patients in system 3 or 6 do not send lab values at all, exclude these
  df_studypop <- df_studypop[!(Systeem == 3 | Systeem == 6), ]
  
  # add transformations of variables
  df_studypop[, `:=`(
    age_square = age^2,
    gfr_square = gfr^2,
    gfr_prev_square = gfr_prev^2,
    gfr_next_square = gfr_next^2,
    log_crp = log(crp),
    log_crp_prev = log(crp_prev),
    log_crp_next = log(crp_next),
    log_nonhdl = log(nonhdl),
    log_nonhdl_prev = log(nonhdl_prev),
    log_nonhdl_next = log(nonhdl_next),
    years_since_first_ascvd_square = years_since_first_ascvd^2,
    maleSex = sex == "M"
  )]
  
  return(df_studypop)
  
  
}