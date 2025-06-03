library(mice)

do_mice <- function(df_studypop, m_imputations, maxit, delta_scale) {
  
  predictorMatrix <- as.matrix(fread("config/predmatrix.csv"), rownames = T)
  
  # prepare event indicators w cumhazards, these go in the imputation model
  df_studypop[, ind_event := status == "event"]
  df_studypop[, ind_com_event := status == "competing_event"]
  df_studypop[, cumhaz1 := 
                nelsonaalen(
                  data = df_studypop[, .(time = round(time, 2), ind_event)],
                  timevar = time,
                  statusvar = ind_event
                )
  ]
  df_studypop[, cumhaz2 := 
                nelsonaalen(
                  data = df_studypop[, .(time = round(time, 2), ind_com_event)],
                  timevar = time,
                  statusvar = ind_com_event
                )
  ]
  
  # select variables which are required for all next steps (imputing and/or result analysis)
  df_studypop <- df_studypop[, .(ind_event, cumhaz1, ind_com_event, cumhaz2, sbp, gfr, gfr_square, 
                             nonhdl, log_nonhdl, 
                             sbp_prev, sbp_next, 
                             gfr_prev, gfr_next,
                             gfr_prev_square, gfr_next_square,
                             nonhdl_prev, nonhdl_next,
                             log_nonhdl_prev, log_nonhdl_next,
                             has_cad, has_pad, has_aaa, has_cevd,
                             diabetes, psych, 
                             on_antithrombotic, 
                             age, age_square, smoker, maleSex,
                             years_since_first_ascvd,
                             years_since_first_ascvd_square, 
                             bmi, vehp_scaled, herkomst,
                             status, time, ses_class
  )]
  df_studypop[, herkomst := factor(herkomst)]

  # set methods. all transformations should be determined from the original value
  meth <- make.method(df_studypop)
  meth["gfr_square"] <- "~ I(gfr^2)"
  meth["gfr_prev_square"] <- "~ I(gfr_prev^2)"
  meth["gfr_next_square"] <- "~ I(gfr_next^2)"
  
  meth["log_nonhdl"] <- "~ I(log(nonhdl))"
  meth["log_nonhdl_prev"] <- "~ I(log(nonhdl_prev))"
  meth["log_nonhdl_next"] <- "~ I(log(nonhdl_next))"
  
  
  imp.all <- vector("list", length(delta_scale))
  ini <- mice(df_studypop, 
              method = meth, 
              predictorMatrix = predictorMatrix,
              m = m_imputations,
              maxit = 0,
              seed = 1234)
  post <- ini$post
  
  for (i in 1:length(delta_scale)) {
    d <- delta_scale[i]
    cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] *", d)
    post["nonhdl"] <- cmd
    imp <- mice(
      df_studypop, 
      method = meth, 
      predictorMatrix = predictorMatrix,
      m = m_imputations,
      maxit = maxit,
      post = post,
      seed = 1234
    )
    imp.all[[i]] <- imp
  }
  
  imp.all[]
  all_imp <- rbindlist(
    lapply(imp.all,
           FUN = function(x) complete(x, action = "long")
    ), idcol = "delta"
  )
  all_imp[, delta := delta_scale[delta]]
  
  return(all_imp)
}


clean_imputations <- function(df_imputations_long, df_studypop) {
  df_imputations_long[, crp := 2]
  df_imputations_long[, log_crp := log(2)]
  df_imputations_long[, outcome := fcase(
    status == "event", 1,
    status == "censored", 0,
    status == "competing_event", 2
  )]
  df_imputations_long[, status := factor(status, levels = c("censored", "event", "competing_event"))]
  df_imputations_long[, RINPERSOON := rep(df_studypop$RINPERSOON, nrow(df_imputations_long)/nrow(df_studypop))]
  df_imputations_long[, psych_epa := RINPERSOON %in% df_studypop[psych_epa == T, RINPERSOON]]
  return(df_imputations_long[])
}
