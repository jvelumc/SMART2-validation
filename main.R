source("src/make_studypop.R")
source("src/impute.R")
source("src/SMART2_predict.R")
source("src/analyse_and_pool.R")
source("src/calibration_plot.R")
source("src/functions.R")

df_studypop <- make_studypop(use_haga = T)

df_imputed <- do_mice(df_studypop, m_imputations = 50, maxit = 25, delta = c(0.9,1, 1.1))
df_imputed <- clean_imputations(df_imputed, df_studypop)
df_imputed[, cvd_risk_10 := predict_smart2(df_imputed)]

df_results <- analyse_and_pool(df_imputed)

df_calplots <- make_calibration_plot(df_imputed[delta == 1 & .imp == 1, ])



df_studypop_2 <- make_studypop(use_haga = F)

df_imputed_2 <- do_mice(df_studypop_2, m_imputations = 50, maxit = 25, delta = 1)
df_imputed_2 <- clean_imputations(df_imputed_2, df_studypop_2)
df_imputed_2[, cvd_risk_10 := predict_smart2(df_imputed_2)]

df_results_2 <- analyse_and_pool(df_imputed_2)


# Table1

df_studypop[, n_vulnerabilities := 0]
df_studypop[ses_class == "low", n_vulnerabilities := n_vulnerabilities + 1]
df_studypop[psych == T, n_vulnerabilities := n_vulnerabilities + 1]
df_studypop[herkomst == "NietWesters", n_vulnerabilities := n_vulnerabilities + 1]
df_studypop[, .N, by = n_vulnerabilities]

psych_cols <- colnames(df_studypop)[grepl("icpc_|med_", colnames(df_studypop))]

df_table1 <- table1(df_studypop, c("age", "bmi", "maleSex", 
                                "smoker", "diabetes", "on_antithrombotic",
                                "has_cad", "has_pad", "has_cevd", "has_aaa", 
                                "cholesterol", "hdl", "nonhdl", "sbp", "gfr", "psych", psych_cols,  
                                "ses_class", "herkomst", "n_vulnerabilities", "time", "sbp_prev", "sbp_next", 
                                "gfr_prev", "gfr_next", "nonhdl_prev", "nonhdl_next"))

# Comparison missingness % Haga records/only ELAN records
df_table2 <- table1(df_studypop_2, c("cholesterol", "hdl", "nonhdl", "sbp", "gfr"))


save_results(df_results, "results_main.xlsx")
save_results(df_calplots, "calplots.xlsx")
save_results(df_results_2, "results_elan.xlsx")
save_results(df_table1, "table1.xlsx")
save_results(df_table2, "table2.xlsx")

# follow up reverse KM
df_studypop[, reverse_event := fifelse(status == "censored", 1, 0)]
print("FU:")
print((
  survfit(Surv(time, reverse_event) ~ 1, data = df_studypop) |> 
    quantile(probs = c(0.25, 0.5, 0.75))
)$quantile)

# event rate
print("event rate")
df_studypop[, sum(status == "event") / sum(time) * 1000]
print("competing event rate")
df_studypop[, sum(status == "competing_event") / sum(time) * 1000]

# observed 10-year event risk
print("observed 10 year event risk")
df_studypop[, outcome := fcase(
  status == "event", 1,
  status == "censored", 0,
  status == "competing_event", 2
)]
predict(
  prodlim(
    formula = Hist(time, outcome) ~ 1, 
    data = df_studypop, 
    type = "risk"
  ), 
  times = 10
)
