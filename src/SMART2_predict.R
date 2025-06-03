predict_smart2 <- function(df_events) {
  coefficients <- fread("config/smart2_coefficients.csv")
  variables <- coefficients$variable
  
  linear_predictor <- (as.matrix(df_events[, ..variables]) %*% coefficients$coefficient)[, 1]
  
  # these coefficients are not estimated from our data but are taken from
  # Hageman et al. Estimation of recurrent atherosclerotic cardiovascular event
  # risk in patients with established cardiovascular disease: the updated
  # SMART2 algorithm (2022):
  mean_LP <- -0.0463729
  baseline_risk_10 <- 0.165822797
  ratio_region <- 0.81590
  computed_risk <- 1 - (1 - baseline_risk_10)^(exp(linear_predictor - mean_LP - log(ratio_region)))
  
  return(computed_risk)
}