library(arrow)
library(dplyr)
library(data.table)

add_smokers <- function(df_studypop) {
  
  df_smokers <- open_dataset(FP_ELAN_SMOKERS) |> 
    filter(RINPERSOON %in% df_studypop$RINPERSOON) |> 
    collect() |> 
    setDT()
  
  df_studypop[, smoker := (
    df_smokers[df_studypop, 
               on = .(RINPERSOON = RINPERSOON, date <= T_prediction_moment ),
               .(RINPERSOON, smokes)][
                 , .SD[.N], by = RINPERSOON
               ][is.na(smokes) | smokes == "stopped", smokes := "no"][, smokes == "yes"]
  )]
}

