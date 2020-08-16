#library(tidyverse)
df_raw <- read_csv("data/immo_data.csv")

# Nicht benotigte Spalte entfernen
df_clean_cols <- subset(df_raw, select = -c(
telekomTvOffer
,telekomHybridUploadSpeed
,newlyConst
,picturecount
,pricetrend
,telekomUploadSpeed
,scoutId
,noParkSpaces
,firingTypes
,yearConstructedRange
,houseNumber
,condition
,interiorQual
,petsAllowed
,street
,streetPlain
,baseRentRange
,thermalChar
,numberOfFloors
,noRoomsRange
,livingSpaceRange
,description
,facilities
,heatingCosts
,energyEfficiencyClass
,lastRefurbish
,electricityBasePrice
,electricityKwhPrice
,totalRent
,geo_bln
,geo_krs
))

### Bereinigung von Datensaetzen
df_clean_na <- na.omit(df_clean_cols) # NAs

write.csv2(df_clean_na, file = "data/immo_scout_cleaned_cols_no_na.csv") # Zwischenspeichern

df_clean_1 <- subset(df_clean_na, floor < 64) # Hoechste Gebaude in DE hat 63 Stockwerke
df_clean_2 <- subset(df_clean_1, noRooms > 0) # Es muss mindesten ein Zimmer vorhanden sein
df_clean_3 <- subset(df_clean_2, livingSpace > 0) # Wohnflaeche muss gueltig sein
df_clean_4 <- subset(df_clean_3, yearConstructed > 1214) # Das aelteste Haus in DE ist 1215
df_clean_4 <- subset(df_clean_4, yearConstructed < 2021) # Erhebung war in 2018 und 2019. Vielleicht wurden Wohnung angeboten die in 2020 fertigstellt werden
df_clean_5 <- subset(df_clean_4, baseRent > 0) # Miete muss groesser 0 sein
df_clean_6 <- subset(df_clean_5, serviceCharge > 0) # Nebenkosten muessen groeßer sein
df_clean_7 <- subset(df_clean_6, serviceCharge != baseRent) # Nebenkosten und Miete duerfen nicht gleich sein
df_immo_cleaned <- df_clean_7

rm(list = c("df_clean_1","df_clean_2","df_clean_3","df_clean_4","df_clean_5","df_clean_6","df_clean_7","df_clean_na","df_clean_cols"))


# Finales Dataset speichern
write.csv2(df_immo_cleaned, file = "GER/immo_scout_cleaned_final.csv")
write.csv2(df_immo_cleaned, file = "data/immo_scout_cleaned_final.csv")


# df_immo_cl_reduced <- subset(df_immo_cleaned, select = c(
# serviceCharge
# ,baseRent
# ,livingSpace
# ,noRooms
# ,floor
# ,yearConstructed
# ))
# 
# 
# df_cor_test <- subset(df_immo_cleaned, grepl("Berlin", df_immo_cleaned[['regio1']]), select = c(
#   serviceCharge
#   ,baseRent
#   ,livingSpace
#   ,noRooms
#   ,floor
#   ,yearConstructed
# ))


## Test
#fitEstateAll_i <- lm(df_immo_cleaned, formula = baseRent ~ (regio1 * regio2) +(livingSpace * yearConstructed * noRooms * floor) +heatingType + balcony  + hasKitchen + cellar + lift + typeOfFlat +  garden)
