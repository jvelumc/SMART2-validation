get_all_incl_events <- function(rin_list) {
  
  read_incl_events <- function(rin_list) {
    # read all hospital diagnoses leading to inclusion
    get_codes <- function() {
      # read in the codes for inclusion events, and process into usable form
      df_codes_icd <- fread("config/codes_inclusion.csv")
      
      df_codes_all <- (rbindlist(
        list(
          df_codes_icd[, .(category, disease, code_type = "ICD9", 
                           code = ICD9, omschrijving = icd_9_omschrijving)],
          df_codes_icd[, .(category, disease, code_type = "ICD10", 
                           code = ICD10, omschrijving = icd_10_omschrijving)]
        )
      ))
      
      df_codes_all <- df_codes_all[code != ""]
      
      df_codes_all[, code_no_dot := sub(".", "", code, fixed = T)]
      
      df_codes_unique <- unique(df_codes_all, by = "code_no_dot")
      return(df_codes_unique)
    }
    
    get_lmr <- function(rin_list, df_codes_all) {
      # read events from LMR (2000-2012) from patients in rin_list, using codes
      # from df_codes_all
      df_lmr <- open_dataset(FP_LMR) |> 
        filter(
          RINPERSOON %in% rin_list,
          (icd10hfddiag %in% df_codes_all$code_no_dot |
             hfddiag %in% df_codes_all$code_no_dot)
        ) |> 
        collect() |> 
        setDT(key = "RINPERSOON")
      
      df_lmr[, code := fifelse(hfddiag %in% df_codes_all$code_no_dot, hfddiag, icd10hfddiag)]
      
      return(df_lmr[, .(RINPERSOONS, RINPERSOON, opndat, code)])
    }
    
    get_lbz <- function(rin_list, df_codes_all) {
      # read events from LBZ (2013-...) from patients in rin_list, using codes
      # from df_codes_all
      df_lbz <- open_dataset(FP_LBZ) |> 
        filter(
          RINPERSOON %in% rin_list,
          LBZIcd10hoofddiagnose %in% df_codes_all$code_no_dot
        ) |> 
        select(
          RINPERSOONS,
          RINPERSOON,
          opndat = LBZOpnamedatum,
          code = LBZIcd10hoofddiagnose
        ) |> 
        collect() |> 
        setDT(key = "RINPERSOON")
      return(df_lbz)
    }
    
    df_codes_all <- get_codes()
    
    df_incl_events <- rbind(
      get_lmr(rin_list, df_codes_all), 
      get_lbz(rin_list, df_codes_all)
    )
    
    df_incl_events <- df_codes_all[df_incl_events, on = .(code_no_dot = code)]
    
    return(df_incl_events[, .(RINPERSOON, category, opndat, type = "diagnose")])
  }
  
  read_surgeries <- function(rin_list) {
    # read all surgeries leading to inclusion
    get_codes <- function() {
      # read in the codes for surgery inclusion events, and process into usable
      # form. Three coding systems were used between 2000 and 2022. ZA is a 
      # subset of CBV.
      
      cvv <- fread(
        input = "config/operatie_codes/cvv_codes.csv", 
        colClasses = "character"
      )
      za <- fread(
        input = "config/operatie_codes/za_codes.csv", 
        colClasses = "character"
      )
      cbv <- fread(
        input = "config/operatie_codes/cbv_codes.csv", 
        colClasses = "character"
      )
      
      all_codes <- rbindlist(
        l = list(cvv = cvv, za = za, cbv = cbv),
        idcol = "code_system",
        fill = T
      )
      
      all_codes[, `:=`(
        code_omschrijving = fcoalesce(cvv_omschrijving, cbv_omschrijving, za_omschrijving),
        cvv_omschrijving = NULL,
        cbv_omschrijving = NULL,
        za_omschrijving = NULL
      )]
      
      df_codes_unique <- unique(all_codes, by = "code")
      
      return(df_codes_unique)
    }
    
    get_operaties <- function(rin_list, df_codes) {
      # read in the surgeries leading to inclusion, from lmr and lbz
      df_lmr_operaties <- open_dataset(FP_LMR_operaties) |> 
        filter(
          RINPERSOON %in% rin_list,
          CVV_code %in% df_codes$code
        ) |> 
        select(RINPERSOON, opndat, CVV_code) |> 
        collect() |> 
        setDT()
      
      df_lbz_operaties <- open_dataset(FP_LBZ_operaties) |> 
        filter(
          RINPERSOON %in% rin_list,
          LBZHoofdverrichtingcvv %in% df_codes$code |
            LBZHoofdverrichtingcbv %in% df_codes$code |
            LBZHoofdverrichtingza %in% df_codes$code |
            LBZHoofdverrichting %in% df_codes$code
        ) |> 
        rename(
          opndat = LBZOpnamedatum,
          CVV_code = LBZHoofdverrichtingcvv,
          CBV_code = LBZHoofdverrichtingcbv,
          ZA_code = LBZHoofdverrichtingza,
          hoofd_code = LBZHoofdverrichting
        ) |> 
        collect() |> 
        setDT()
      
      df_operaties <- rbind(df_lmr_operaties, df_lbz_operaties, fill = T)
      
      code_cols <- c("CVV_code", "CBV_code", "ZA_code", "hoofd_code")
      df_operaties[, (code_cols) := 
                     lapply(.SD, function(x) fifelse(x == "", NA_character_, x)),
                   .SDcols = code_cols]
      
      df_operaties[!is.na(CVV_code), code_type := "cvv"]
      df_operaties[!is.na(hoofd_code), code_type := "hoofd"]
      df_operaties[!is.na(ZA_code), code_type := "za"]
      df_operaties[!is.na(CBV_code), code_type := "cbv"]
      df_operaties[!is.na(ZA_code) & !is.na(CBV_code), code_type := "za and cbv"]
      
      # some rows both have CBV code and ZA code. Prioritize CBV but sometimes ZA is needed
      df_operaties[, code := fcoalesce(
        CVV_code, 
        hoofd_code, 
        fifelse(CBV_code %in% df_codes$code, CBV_code, NA_character_), 
        ZA_code)]
      
      # add code description
      df_operaties <- df_codes[df_operaties, on = .(code = code)]
      
      return(df_operaties[, .(RINPERSOON, category, opndat, type = "surgery")])
    }
    
    df_codes <- get_codes()
    df_operaties <- get_operaties(rin_list, df_codes)
    
    return(df_operaties)
  }
  
  
  
  
  df_incl_events <- read_incl_events(rin_list)
  df_operaties <- read_surgeries(rin_list)
  
  df_events <- rbind(
    df_incl_events,
    df_operaties
  )
  return(df_events)
}