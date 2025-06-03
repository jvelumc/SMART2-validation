library(survival)
library(riskRegression)

analyse_and_pool <- function(df_imputations_long) {
  
  delta <- unique(df_imputations_long$delta)
  
  make_subgroup_list <- function(df_imputations_long) {
    subgroup_list <- list(
      "overall" = df_imputations_long,
      "non_psych" = df_imputations_long[psych == F],
      "psych" = df_imputations_long[psych == T],
      "lowses" = df_imputations_long[ses_class == "low"],
      "midses" = df_imputations_long[ses_class == "mid"],
      "highses" = df_imputations_long[ses_class == "high"],
      "dutch" = df_imputations_long[herkomst == "Dutch"],
      "western_migrants" = df_imputations_long[herkomst == "Westers"],
      "nonwestern_migrants" = df_imputations_long[herkomst == "NietWesters"],
      "lowses_and_psych" = df_imputations_long[ses_class == "low" & psych == T],
      "lowses_nonwestern" = df_imputations_long[ses_class == "low" & herkomst == "NietWesters"],
      "nonwestern_psych" = df_imputations_long[herkomst == "NietWesters" & psych == T],
      "epa" = df_imputations_long[psych_epa == T]
    )
    return(subgroup_list)
  }
  
  pool <- function(Q_hat, variance) {
    # Q_hat is vector of estimates of each imputed dataset
    # variance is vector of variance of Q_hat of each imputed dataset 
    Q_bar <- mean(Q_hat)
    m <- length(Q_hat)
    B  <- 1 / ( m - 1 ) * sum( ( Q_hat - Q_bar ) ^ 2 )
    U_bar <- 1 / m * sum(variance)
    Total <- U_bar + ( 1 + 1 / m ) * B
    return(list(mean = Q_bar, var = Total))
  }
  
  get_oe <- function(subgroup) {
    horizon <- 10
    primary_event <- 1
    
    # first get observed of 1st iteration (observed is same in all iterations)
    first_iter <- subgroup[.imp == 1, ]
    obj <- summary(survfit(Surv(time, status) ~ 1, data = first_iter), times = horizon)
    aj <- list("obs" = obj$pstate[, primary_event + 1], "se" = obj$std.err[, primary_event + 1])
    
    results <- list()
    for (i in unique(subgroup[, .imp])) {
      current_iter <- subgroup[.imp == i]
      OE <- aj$obs / mean(current_iter$cvd_risk_10)
      lower <- exp(log(OE) - qnorm(0.975) * aj$se / aj$obs)
      upper <- exp(log(OE) + qnorm(0.975) * aj$se / aj$obs)
      var_log_oe <- ( ( log(upper) - log(lower) ) / (2 * qnorm(0.975) ) ) ^ 2
      results[[i]] <- list("logOE" = log(OE), "var" = var_log_oe,
                           "lower" = lower, "upper" = upper)
    }
    results <- rbindlist(results)
    pooled <- pool(results$logOE, results$var)
    OE <- exp(pooled$mean)
    var <- pooled$var
    
    final_results <- list(
      OE = OE,
      lower = exp(log(OE) - qnorm(0.975) * sqrt(var)),
      upper = exp(log(OE) + qnorm(0.975) * sqrt(var))
    )  
    return(final_results)
  }
  
  get_score <- function(df_studypop) {
    horizon <- 10
    primary_event <- 1
    
    score <- Score(
      list(df_studypop[, cvd_risk_10]),
      formula = Hist(time, outcome) ~ 1,
      cens.model = "km",
      data = df_studypop[, .(time, outcome)],
      conf.int = T,
      times = horizon,
      metrics = "AUC",
      cause = primary_event,
    )
  }
  
  get_auc <- function(subgroup) {
    logit <- function(x) {
      log(x) - log(1-x)
    }
    results <- list()
    for (i in unique(subgroup[, .imp])) {
      current_iter <- subgroup[.imp == i]
      score <- get_score(current_iter)
      cub <-  score$AUC$score$upper
      clb <-  score$AUC$score$lower
      est_var <- ((logit(cub) - logit(clb))/(2*1.96))^2
      results[[i]] <- list(
        "AUC" = score$AUC$score$AUC,
        "se" = score$AUC$score$se,
        "var" = score$AUC$score$se^2
      )
    }
    results <- rbindlist(results)
    pooled <- psfmi::pool_auc(results$AUC, results$se, nimp = length(results$AUC), F)
    
    final_results <- list(
      AUC = pooled[2],
      lower = pooled[1],
      upper = pooled[3]
    )
    return(final_results)
  }
  
  get_n <- function(subgroup) {
    list(n = subgroup[.imp == 1, .N], n_events = subgroup[.imp == 1 & status == "event", .N])
  }
  
  analyse_over_all_subgroups <- function(subgroup_list, f) {
    res <- data.table(
      subset = names(subgroup_list), 
      rbindlist(lapply(subgroup_list, f))
    )
  }
  
  make_pretty_result_table <- function(d) {
    subgroup_list_d <- make_subgroup_list(df_imputations_long[delta == d])
    
    n <- analyse_over_all_subgroups(subgroup_list_d, get_n)
    oes <- analyse_over_all_subgroups(subgroup_list_d, get_oe)
    auc <- analyse_over_all_subgroups(subgroup_list_d, get_auc)
    
    rnd <- function(x) {
      sprintf("%.3f", x)
    }
    
    df_results <- n
    df_results[oes, OE := paste0(rnd(OE), " [", rnd(lower), ", ", rnd(upper), "]"), on = "subset"]
    df_results[auc, AUC := paste0(rnd(AUC), " [", rnd(lower), ", ", rnd(upper), "]"), on = "subset"]
    df_results[, delta_parameter := d]
    return(df_results)
  }
  
  df_results <- rbindlist(lapply(as.list(delta), make_pretty_result_table))
  
  return(df_results)
}
