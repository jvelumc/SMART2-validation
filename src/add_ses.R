source("H:/Jasper/datav2/utilities.R")

add_ses <- function(df_events) {
  years <- c(2009:2020)
  
  df_inkomsten_files <- FP_inkomsten()[year %in% years,]
  
  get_koppel <- function(Year) {
    print(Year)
    open_dataset(
      df_inkomsten_files[year == Year, koppel_file]
    ) |> 
      filter(RINPERSOON %in% df_events$RINPERSOON) |> 
      collect() |> 
      setDT()
  }
  
  koppels <- lapply(years, get_koppel)
  
  df_koppel_long <- rbindlist(koppels, idcol = "year")[, year := year + years[1] - 1]
  
  get_inkomst <- function(Year) {
    open_dataset(
      df_inkomsten_files[year == Year, veh_file]
    ) |> 
      filter(RINPERSOONHKW %in% df_koppel_long[year == Year, RINPERSOONHKW]) |> 
      select(RINPERSOONHKW, VEHP100WELVAART) |> 
      collect() |> 
      setDT()
  }
  
  welvs <- lapply(years, get_inkomst)
  df_welvs_long <- rbindlist(welvs, idcol = "year")[, year := year + years[1] - 1]
  
  df_welvaart <- df_welvs_long[df_koppel_long
                               , on = .(year, RINPERSOONHKW)
                               , .(RINPERSOON, year, VEHP100WELVAART)
                               , nomatch = NULL]
  
  uniqueN(df_welvaart$RINPERSOON)
  df_events[, year_before_incl_event := year(incl_event_date) - 1]
  df_events <- df_welvaart[df_events, on = .(RINPERSOON = RINPERSOON, year = year_before_incl_event)]

  df_events[, vehp_scaled := percent_rank(VEHP100WELVAART)*100, by = .(floor(age/5))]
  
  df_events[, ses_class := fcase(
    vehp_scaled <= 33.3, "low", 
    vehp_scaled <= 66.6, "mid",
    default = "high")]
  
  return(df_events)
}
