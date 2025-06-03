add_herkomst <- function(df_studypop) {
  df_gba <- open_dataset(FP_GBAPERS) |> 
    select(RINPERSOON, GBAHERKOMSTLAND) |> 
    filter(RINPERSOON %in% df_studypop$RINPERSOON) |> 
    collect() |> 
    setDT(key = "RINPERSOON")
  
  landdict <- fread("H:/Jasper/datav2/dictionaries/landdict.csv", 
                    select = c("landcode", "LANDAKTUEEL", "LANDTYPE"))
  
  df_gba <- landdict[df_gba, on = .(landcode == GBAHERKOMSTLAND)]
  df_gba[, herkomst := fifelse(LANDAKTUEEL == "Nederland", "Dutch", LANDTYPE)]
  df_gba <- df_gba[, .(RINPERSOON, herkomst)]
  
  df_studypop <- df_gba[df_studypop, on = .(RINPERSOON)]
  return(df_studypop)
}
