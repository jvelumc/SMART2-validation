source("H:/Jasper/datav2/utilities.R")

# WCIA codes worden gebruikt in huisartspraktijken om labonderzoeken te 
# registreren. bijv: code 1744 geeft aan dat het om een bloeddruk meting gaat. 
# Codes 357, 2408 betekent dat de bijbehorende meting gewicht van patient aangeeft
# Code 560 geeft de lengte aan van een patient
# Code 1272 geeft de BMI aan van patient

add_bmi <- function(df_studypop) {
  bmi_data <- open_dataset(FP_ELAN_LAB) |> 
    filter(
      RINPersoon %in% df_studypop$RINPERSOON,
      dWCIANummer %in% c("357", "2408", "560", "1272")
    ) |> 
    select(
      RINPERSOON = RINPersoon, 
      dBepalingdatum, 
      dWCIANummer, 
      dWCIAOmschrijving, 
      Resultaat, 
      Resultaattoevoeging,
      dResultaatnummer
    ) |> 
    collect() |> 
    setDT()
  
  bmi_data[, result := clean_number(Resultaat)]
  
  # get BMI from weight/length^2
  lengtes <- bmi_data[dWCIANummer == "560"]
  lengtes[, result := fifelse(result >= 100, result / 100, result)]
  lengtes <- lengtes[result >= 1.3 & result <= 2.5, .(RINPERSOON, dBepalingdatum, length = result)] # shouldnt be hardcoded
  
  weights <- bmi_data[dWCIANummer %in% c("357", "2408"), ]
  weights[, result := clean_number(Resultaat)]
  weights <- weights[result >= 30 & result <= 350, .(RINPERSOON, dBepalingdatum, weight = result)] # shouldnt be hardcoded
  
  last_length <- lengtes[order(dBepalingdatum), .SD[.N], by = RINPERSOON][, .(RINPERSOON, length)]
  
  bmi <- last_length[weights, on = .(RINPERSOON)]
  bmi[, bmi := weight/length^2]
  bmi[, source := "computed"]
  
  # get BMI when directly filled in
  bmi_filledin <- bmi_data[dWCIANummer == "1272" & result >= 12 & result <= 50, # shouldnt be hardcoded
                           .(RINPERSOON, dBepalingdatum, bmi = result, source = "filledin")]
  
  # combine
  bmi_all <- rbind(bmi[, .(RINPERSOON, dBepalingdatum, bmi, source)], bmi_filledin)
  bmi_all <- bmi_all[!is.na(bmi), ]
  bmi_all[, source := NULL]
  setkey(bmi_all, RINPERSOON, dBepalingdatum)
  
  # take LOCF
  df_studypop <- bmi_all[df_studypop, on = .(RINPERSOON, dBepalingdatum <= lab_window_end),
                         mult = "last"]
  
  return(df_studypop)
}




