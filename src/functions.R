library(openxlsx)

save_results <- function(x, filename) {
  folder <- paste0("results/", Sys.Date())
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  filepath <- paste0(folder, "/", filename)
  
  if (grepl(".csv$", filename)) {
    fwrite(x, filepath)  
  }
  else if (grepl(".xlsx$", filename)) {
    write.xlsx(x, filepath)  
  }
  else {
    stop("no valid file extension")
  }
}


table1 <- function(data, variables) {
  # make a table1 with patient characteristics
  factor_var <-  c()
  numeric_var <- c()
  binary_var <- c()
  
  for (v in variables) {
    if (is.logical(df_studypop[[v]])) {
      binary_var <- c(binary_var, v)
    }
    else if (uniqueN(data[, ..v]) <= 5) {
      factor_var <- c(factor_var, v)
    }
    else {
      numeric_var <- c(numeric_var, v)
    }
  }
  
  summarize <- function(data, v) {
    if (v %in% numeric_var) {
      rows <- 
        data[, .(
          var = v,
          mean = mean(get(v), na.rm = T),
          sd = sd(get(v), na.rm = T),
          median = median(get(v), na.rm = T),
          q1 = quantile(get(v), probs = 0.25,  na.rm = T),
          q3 = quantile(get(v), probs = 0.75, na.rm = T),
          N_missing = sum(is.na(get(v))),
          proportion_missing = mean(is.na(get(v))) * 100
        )]
    }
    if (v %in% binary_var) {
      rows <- 
        data[, .(
          var = paste0(v, ": true"),
          N = sum(get(v), na.rm = T),
          proportion = sum(get(v), na.rm = T)/.N * 100,
          N_missing = sum(is.na(get(v))),
          proportion_missing = mean(is.na(get(v))) * 100
        )]
    }
    if (v %in% factor_var) {
      
      levels <- unique(data[[v]])
      
      summarize_level <- function(data, v, level) {
          row <- data[, .(
            var = paste0(v, ": ", level),
            N = sum(get(v) == level, na.rm = T),
            proportion = sum(get(v) == level, na.rm = T)/.N * 100,
            N_missing = sum(is.na(get(v))),
            proportion_missing = mean(is.na(get(v))) * 100
          )]
          return(row)
      }
      rows <- rbindlist(lapply(levels, function(l) summarize_level(data, v, l)))
    }
    return(rows)
  }
  table1 <- rbind(
    data.table(var = "Total population size", N = nrow(data), proportion = 100),
    rbindlist(
      lapply(variables, function(v) summarize(data, v)),
      fill = T
    ),
    fill = T
  )
  t1_num_cols <- colnames(Filter(is.numeric, table1))
  table1[, c(t1_num_cols) := lapply(.SD, function(x) round(x, digits = 1)), .SDcols = t1_num_cols]
  return(table1[])
}
