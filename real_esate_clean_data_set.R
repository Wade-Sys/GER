library(tidyverse)
library(broom)
library(rgdal)
library(sp)
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
,regio3
,date
))

#drop raw
rm(list = c("df_raw"))
### Bereinigung von Datensaetzen
df_clean_na <- na.omit(df_clean_cols) # NAs

write.csv2(df_clean_na, file = "data/immo_scout_cleaned_cols_no_na.csv",row.names = FALSE) # Zwischenspeichern

df_clean_1 <- subset(df_clean_na, floor < 64) # Hoechste Gebaude in DE hat 63 Stockwerke
df_clean_2 <- subset(df_clean_1, noRooms > 0) # Es muss mindesten ein Zimmer vorhanden sein
df_clean_3 <- subset(df_clean_2, livingSpace > 0) # Wohnflaeche muss gueltig sein
df_clean_4 <- subset(df_clean_3, yearConstructed > 1214) # Das aelteste Haus in DE ist 1215
df_clean_4 <- subset(df_clean_4, yearConstructed < 2021) # Erhebung war in 2018 und 2019. Vielleicht wurden Wohnung angeboten die in 2020 fertigstellt werden
df_clean_5 <- subset(df_clean_4, baseRent > 0) # Miete muss groesser 0 sein
df_clean_5 <- subset(df_clean_5, baseRent != 999) # unplasible Werte
df_clean_5 <- subset(df_clean_5, baseRent != 9999) # unplasible Werte
df_clean_5 <- subset(df_clean_5, baseRent != 99999) # unplasible Werte
df_clean_6 <- subset(df_clean_5, serviceCharge > 0) # Nebenkosten muessen groeßer sein
df_clean_7 <- subset(df_clean_6, serviceCharge != baseRent) # Nebenkosten und Miete duerfen nicht gleich sein
# Unterstriche durch Leerzeichen ersetzen
df_clean_8 <- df_clean_7
df_clean_8$regio1 <- str_replace_all(df_clean_8$regio1,"_","-")
df_clean_8$regio2 <- str_replace_all(df_clean_8$regio2,"_","-")
df_clean_8$regio3 <- str_replace_all(df_clean_8$regio3,"_"," ")
df_clean_8$heatingType <- str_replace_all(df_clean_8$heatingType,"_"," ")
df_clean_8$typeOfFlat <- str_replace_all(df_clean_8$typeOfFlat,"_"," ")
# Spalten uebersetzen: Heizungsart
df_clean_9 <- df_clean_8
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"self contained central heating","autonome Zentralheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"central heating","Zentralheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"floor heating","Fußbodenheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"district heating","Fernwärme")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"oil heating","Ölheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"gas heating","Gasheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"electric heating","elektrische Heizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"combined heat and power plant","Kraft-Wärme-Kopplungsanlage")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"heat pump","Wärmepumpe")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"night storage heater","Nachtspeicherheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"wood pellet heating","Holzpellet-Heizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"stove heating","Ofenheizung")
df_clean_9$heatingType <- str_replace_all(df_clean_9$heatingType,"solar heating","Solarheizung")
# Spalten uebersetzen: Wohnungstyp
df_clean_10 <- df_clean_9
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"raised ground floor","Hochparterre")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"ground floor","Erdgeschoss")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"apartment","Wohnung")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"other","sonstige")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"roof storey","Dachgeschoss")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"terraced flat","Terrassenwohnung")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"maisonette","Maisonette")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"penthouse","Penthouse")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"half basement","Souterrain")
df_clean_10$typeOfFlat <- str_replace_all(df_clean_10$typeOfFlat,"loft","Loft")
# Landkreise bereinigen
df_clean_11 <- df_clean_10
# ACHTUNG BIS HIER HER AUSUEHREN UND DANN das landkreise_cleaning.R ausfuehren und dann weiter
# Endgueltiger Datensatz
df_immo_cleaned <- df_clean_11

#clean data
rm(list = c("df_clean_1","df_clean_2","df_clean_3","df_clean_4","df_clean_5","df_clean_6","df_clean_7","df_clean_8","df_clean_9", "df_clean_10","df_clean_11",
            "df_clean_na","df_clean_cols"))


# Finales Dataset speichern
write.csv2(df_immo_cleaned, file = "GER/immo_scout_cleaned_final.csv", row.names = FALSE)
write.csv2(df_immo_cleaned, file = "data/immo_scout_cleaned_final.csv", row.names = FALSE)

## -------------------------------------------------------------------------------------------------------------------------------------------------
## Prepare geodata

filePath <- paste0(getwd(),"/GER/geodata/vg2500")
gerKrsMap <- readOGR(dsn=filePath, layer="vg2500_krs")
gerLanMap <- readOGR(dsn=filePath, layer="vg2500_lan")

gerKrsWithId <- data.frame(c(seq(0,400)),gerKrsMap@data[["GEN"]])
gerLanWithId <- data.frame(c(seq(0,15)),gerLanMap@data[["GEN"]])

names(gerKrsWithId) <- c("id", "landkreis")
names(gerLanWithId) <- c("id", "bundesland")

mapLandkreis <- merge(x=tidy(gerKrsMap), y=gerKrsWithId, by="id")
mapBundesland <- merge(x=tidy(gerLanMap), y=gerLanWithId, by="id")

write.csv2(mapLandkreis, file = "GER/geo_landkreis.csv", row.names = FALSE)
write.csv2(mapBundesland, file = "GER/geo_bundesland.csv", row.names = FALSE)

rm(list = c("filePath", "gerKrsMap","gerLanMap","gerKrsWithId","gerLanWithId"))#,"mapLandkreis","mapBundesland"))
