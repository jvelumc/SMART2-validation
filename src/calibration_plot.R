make_calibration_plot <- function(df_events) {
  
  library(ggplot2)
  library(prodlim)
  
  primary_event <- 1
  horizon <- 10
  
  n_groups <-8
  
  make_calibration_plot <- function(data, title) {
    
    data <- copy(data)
    data <- data[order(cvd_risk_10), ]
    data <- data[, group := (.I-1)%/%(.N/n_groups)]
    
    get_ci <- function(data) {
      prodlim <- prodlim(formula = Hist(time, outcome) ~ 1, data = data)
      meanobs <- stepfun(prodlim$time, c(0, prodlim$cuminc$`1`))(10)
      se <- stepfun(prodlim$time, c(0, prodlim$se.cuminc$`1`))(10)
      return(list("meanobs" = meanobs, "se" = se))
    }
    deciles <- data[, .(meanpred = mean(cvd_risk_10), 
                        meanobs = get_ci(.SD)[["meanobs"]],
                        se = get_ci(.SD)[["se"]],
                        N = .N), by = group]
    deciles[, `:=`(ci_lower = pmax(0, meanobs - 1.96 * se), ci_upper = pmin(1, meanobs + 1.96 * se))]
    
    risk_to_percentage <- function(x) {
      paste0(as.numeric(x)*100, "%")
    }
    
    deciles |> 
      ggplot(aes(x=meanpred, y = meanobs)) +
      geom_point() + 
      geom_line() + 
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) + 
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
      scale_x_continuous(expand = c(0,0), limits = c(0,0.6),
                         labels = risk_to_percentage) +
      scale_y_continuous(expand = c(0,0), limits = c(0,0.6), labels = risk_to_percentage) +
      xlab("Predicted 10-year risk") + 
      ylab("Observed 10-year risk") + 
      ggtitle(title) +
      theme_classic()
    
    
    ggsave(filename = paste0("results/calibration_", title, ".png"), width = 1500, 
           height = 1500, units = "px")
    
    deciles[, subgroup := title]
    return(deciles)
    
  }
  
  make_subgroup_list <- function(df_events) {
    subgroup_list <- list(
      "overall" = df_events,
      "non_psych" = df_events[psych == F],
      "psych" = df_events[psych == T],
      "lowses" = df_events[ses_class == "low"],
      "midses" = df_events[ses_class == "mid"],
      "highses" = df_events[ses_class == "high"],
      "dutch" = df_events[herkomst == "Dutch"],
      "western_migrants" = df_events[herkomst == "Westers"],
      "nonwestern_migrants" = df_events[herkomst == "NietWesters"]
      # "lowses_and_psych" = df_events[ses_class == "low" & psych == T],
      # "lowses_nonwestern" = df_events[ses_class == "low" & herkomst == "NietWesters"]
      # "nonwestern_psych" = df_events[herkomst == "NietWesters" & psych == T],
      # "epa" = df_events[psych_epa == T]
    )
    return(subgroup_list)
  }
  
  subgroup_list <- make_subgroup_list(df_events)
  
  calibration_plots <- rbindlist(lapply(
    names(subgroup_list), 
    function(x) make_calibration_plot(subgroup_list[[x]], x)
  ))
  return(calibration_plots)
}

