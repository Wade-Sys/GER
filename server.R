# Bibliotheken laden.
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(tidyverse)
library(broom)
library(maps)
library(mapdata)
library(rgdal)
library(sp)
library(mapproj)

# Daten laden.
df_immo_cleaned <- read_csv2(file = "data/immo_scout_cleaned_final.csv")
dfBundesland <- read_csv2(file = "data/geo_bundesland.csv")
dfLandkreis <- read_csv2(file = "data/geo_landkreis.csv")


# Server Logik.
shinyServer(function(input, output, session) {
    print(getwd())
    # Reaktive Datensaetze vorbereiten
    data_immo <- reactive(df_immo_cleaned)
    bundeslaender <- reactive(unique(sort(data_immo()$regio1)))
    landkreiseAll <- reactive(unique(data_immo()[c('regio1','regio2')]))
    typeOfFlat <- reactive(unique(sort(data_immo()$typeOfFlat)))
    heatingType <- reactive(unique(sort(data_immo()$heatingType)))
    data_immo_subset_db <- reactive(0)
    immoDashboardStatistics <- reactive(0)
    
    # Deutschlandkarte fuer das Dashboard initial erstellen.
    mapGerman <- ggplot() + geom_polygon(data = dfBundesland, aes( x = long, y = lat, group = group), fill="#00c0ef", color="#f49c68") + theme_void()
    
    # Boolesche Werte fuer die Tabelle mit JA/NEIN ersetzen.
    exploreImmoDataTable <- df_immo_cleaned
    exploreImmoDataTable$balcony <- ifelse(exploreImmoDataTable$balcony == TRUE, "Ja", "Nein")
    exploreImmoDataTable$cellar <- ifelse(exploreImmoDataTable$cellar == TRUE, "Ja", "Nein")
    exploreImmoDataTable$lift <- ifelse(exploreImmoDataTable$lift == TRUE, "Ja", "Nein")
    exploreImmoDataTable$hasKitchen <- ifelse(exploreImmoDataTable$hasKitchen == TRUE, "Ja", "Nein")
    exploreImmoDataTable$garden <- ifelse(exploreImmoDataTable$garden == TRUE, "Ja", "Nein")
    
    ## ---------------------------------------------------------------------------------------------------------------------------------
    ## Dashboard-Tab: dashboard
    ## ---------------------------------------------------------------------------------------------------------------------------------
   
    # Dropdown: Bundesland; Hinzufuegen der "Alle" option,
    observe(priority = 100, {
      updateSelectInput(session = session, inputId = "dbBundesland", choices = c("Alle",bundeslaender()))
    })
    
    # Dropdown: Landkreis; Hinzufuegen der "Alle" option. Auswahlmoeglichkeiten nach Bundesland einschraenken.
    observe(priority = 100, {
      if(input$dbBundesland == 'Alle') {
        dbLkSelected <- subset(landkreiseAll(), select = c("regio2"))
        
      } else {
        dbLkSelected <- subset(landkreiseAll(), grepl(paste0('^',input$dbBundesland,'$'), landkreiseAll()$regio1),select = c("regio2"))
      }
      
      updateSelectInput(session = session, inputId = "dbLandkreis", choices = c("Alle",sort(unique(dbLkSelected$regio2))))
      
    })
    
    # Werte und Diagramme im Dashboard je nach ausgewaehlten Bundesland und Landkreis berechnen.
    observe(priority = 100, {
      
      # Ersetzungesvariable setzen.
      greplInputBundesland <- paste0('^',input$dbBundesland,'$')
      greplInputLandkreis <- paste0('^',input$dbLandkreis,'$')
      greplInputLandkreis <- str_replace_all(str_replace_all(greplInputLandkreis,"\\(","\\\\("),"\\)","\\\\)") # Klammer muessen fuer die Suche ersetzt werden.
      
      
      # Alle Datensaetze fuer den ausgewaehlten Landkreis und Bundesland zum Berechnen extrahieren und die Geo-Daten vorbereiten.
      if((input$dbBundesland == "Alle" && input$dbLandkreis == "Alle") || (input$dbBundesland == "" && input$dbLandkreis == "")){
        data_immo_subset_db <- data_immo()
        
        mapGerman <- mapGerman + geom_polygon(data = dfBundesland, 
                                aes( x = long, y = lat, group = group), fill="#00c0ef", color="#f49c68") + theme_void()
      }
      else if(input$dbBundesland == "Alle" && input$dbLandkreis != "Alle") {
        data_immo_subset_db <- subset(data_immo(), grepl(greplInputLandkreis, data_immo()$regio2))
        
        mapGerman <- mapGerman + geom_polygon(data = subset(dfLandkreis, 
                                grepl(greplInputLandkreis,dfLandkreis$landkreis)), 
                                aes( x = long, y = lat, group = group),fill = "#f49c68", color="grey") + theme(legend.position = "none")
      }
      else if(input$dbBundesland != "Alle" && input$dbLandkreis == "Alle") {
        data_immo_subset_db <- subset(data_immo(), grepl(greplInputBundesland, data_immo()$regio1))
        
        mapGerman <- mapGerman + geom_polygon(data = subset(dfBundesland, 
                                grepl(greplInputBundesland,dfBundesland$bundesland)), 
                                aes( x = long, y = lat, group = group), fill = "#f49c68", color="grey") + theme(legend.position = "none")
      }
      else if(input$dbBundesland != "Alle" && input$dbLandkreis != "Alle") {
         data_immo_subset_db <- subset(data_immo(), grepl(greplInputBundesland, data_immo()$regio1) & grepl(greplInputLandkreis, data_immo()$regio2))
         
         mapGerman <- mapGerman + geom_polygon(data = subset(dfLandkreis, 
                                grepl(greplInputLandkreis,dfLandkreis$landkreis)), 
                                aes( x = long, y = lat, group = group), fill = "#f49c68", color="grey") + theme(legend.position = "none")
      }
      else {
        mapGerman <- mapGerman + geom_polygon(data = dfBundesland, 
                                aes( x = long, y = lat, group = group), fill="#00c0ef", color="#f49c68") + theme_void()
      }
      
      # Desktriptive Statistken berechnen.
      immoDashboardStatistics <- reactive(computeDashboardStatistics(data_immo_subset_db,fields = "all"))
      
      #DEBUG: Ergebnisse auf der Konsole ausgeben.
      #print(immoDashboardStatistics())


      # Berechnete Statistiken in den Info-Boxen anzeigen.
      
      # Stasistiken Info-Box: BaseRent (Kaltmiete).
      output$dbInfoBoxBaseRentAvg <- renderInfoBox({
        infoBox(title = "Arithmetisches Mittel", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","avg"], " €"), icon = icon("euro-sign"), fill = TRUE)
      })
      output$dbInfoBoxBaseRentMedian <- renderInfoBox({
        infoBox(title = "Median", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","median"], " €"),icon = icon("euro-sign"), fill = TRUE)
      })
      output$dbInfoBoxBaseRentMin <- renderInfoBox({
        infoBox(title = "Min-Wert", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","min"], " €"),icon = icon("euro-sign"), fill = TRUE)
      })
      output$dbInfoBoxBaseRentMax <- renderInfoBox({
        infoBox(title = "Max-Wert", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","max"], " €"),icon = icon("euro-sign"), fill = TRUE)
      })
      output$dbInfoBoxBaseRentQ25 <- renderInfoBox({
        infoBox(title = ".25-Quantil", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","q25"], " €"), icon = icon("euro-sign"), fill = TRUE)
      })
      output$dbInfoBoxBaseRentQ75 <- renderInfoBox({
        infoBox(title = ".75-Quantil", value = paste0(immoDashboardStatistics()["dfSummaryBaseRent","q75"], " €"), icon = icon("euro-sign"), fill = TRUE)
      })
      # Boxplot: BaseRent (Kaltmiete).
      output$dbBaseRentBoxplot <- renderPlot({
         ggplot(as.data.frame(data_immo_subset_db), aes(x = baseRent)) + 
          geom_boxplot(outlier.fill = "#f49c68",outlier.colour = "#000000", outlier.shape = 21,outlier.size = 2, fill = "#009abf", colour = "#f49c68") +
          scale_x_continuous(trans = "log10") +
          annotation_logticks(sides = "b") +
          xlab("Kaltmiete in €") +
          theme_bw(base_size = 22,base_line_size = 22/44) +
          theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
      })
      
      
      # Stasistiken Info-Box: LivingSpace (Wohnflaeche).
      output$dbInfoBoxLivingSpaceAvg <- renderInfoBox({
        infoBox(title = "Arithmetisches Mittel", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","avg"], " qm"), icon = icon("ruler-combined"), fill = TRUE)
      })
      output$dbInfoBoxLivingSpaceMedian <- renderInfoBox({
        infoBox(title = "Median", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","median"], " qm"),icon = icon("ruler-combined"), fill = TRUE)
      })
      output$dbInfoBoxLivingSpaceMin <- renderInfoBox({
        infoBox(title = "Min-Wert", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","min"], " qm"),icon = icon("ruler-combined"), fill = TRUE)
      })
      output$dbInfoBoxLivingSpaceMax <- renderInfoBox({
        infoBox(title = "Max-Wert", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","max"], " qm"),icon = icon("ruler-combined"), fill = TRUE)
      })
      output$dbInfoBoxLivingSpaceQ25 <- renderInfoBox({
        infoBox(title = ".25-Quantil", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","q25"], " qm"),icon = icon("ruler-combined"), fill = TRUE)
      })
      output$dbInfoBoxLivingSpaceQ75 <- renderInfoBox({
        infoBox(title = ".75-Quantil", value = paste0(immoDashboardStatistics()["dfSummaryLivingSpace","q75"], " qm"),icon = icon("ruler-combined"), fill = TRUE)
      })
      # Boxplot: LivingSpace (Wohnflaeche).
      output$dbLivingSpaceBoxplot <- renderPlot({
        ggplot(as.data.frame(data_immo_subset_db), aes(x = livingSpace)) + 
          geom_boxplot(outlier.fill = "#f49c68",outlier.colour = "#000000", outlier.shape = 21,outlier.size = 2, fill = "#009abf", colour = "#f49c68") +
          scale_x_continuous(trans = "log2",breaks = c(seq(20,50,10),seq(60,100,15),seq(150,600,200))) +
          xlab("Wohnfläche in qm") +
          theme_bw(base_size = 22,base_line_size = 22/44) +
          theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
      })
      
      
      # Stasistiken Info-Box: noRooms (Anzahl Zimmer).
      output$dbInfoBoxNoRoomsAvg <- renderInfoBox({
        infoBox(title = "Arithmetisches Mittel", value = immoDashboardStatistics()["dfSummaryNoRooms","avg"], icon = icon("building"), fill = TRUE)
      })
      output$dbInfoBoxNoRoomsMedian <- renderInfoBox({
        infoBox(title = "Median", value = immoDashboardStatistics()["dfSummaryNoRooms","median"], icon = icon("building"), fill = TRUE)
      })
      output$dbInfoBoxNoRoomsMin <- renderInfoBox({
        infoBox(title = "Min-Wert", value = immoDashboardStatistics()["dfSummaryNoRooms","min"], icon = icon("building"), fill = TRUE)
      })
      output$dbInfoBoxNoRoomsMax <- renderInfoBox({
        infoBox(title = "Max-Wert", value = immoDashboardStatistics()["dfSummaryNoRooms","max"], icon = icon("building"), fill = TRUE)
      })
      output$dbInfoBoxNoRoomsQ25 <- renderInfoBox({
        infoBox(title = ".25-Quantil", value = immoDashboardStatistics()["dfSummaryNoRooms","q25"], icon = icon("building"), fill = TRUE)
      })
      output$dbInfoBoxNoRoomsQ75 <- renderInfoBox({
        infoBox(title = ".75-Quantil", value = immoDashboardStatistics()["dfSummaryNoRooms","q75"], icon = icon("building"), fill = TRUE)
      })
      # Boxplot: noRooms (Anzahl Zimmer).
      output$dbNoRoomsBoxplot <- renderPlot({
        ggplot(as.data.frame(data_immo_subset_db), aes(x = noRooms)) + 
          geom_boxplot(outlier.fill = "#f49c68",outlier.colour = "#000000", outlier.shape = 21,outlier.size = 2, fill = "#009abf", colour = "#f49c68") +
          scale_x_continuous(breaks = seq(1,15,0.5)) +
          xlab("Anzahl der Zimmer") +
          theme_bw(base_size = 22,base_line_size = 22/44) +
          theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
      })
      
      
      # Deutschlandkarte auf Basis den ausgewaehlten Daten zeichen.
      output$dbPlotMapGermany <- renderPlot({
        mapGerman
      })
      
    })
    
    ## ---------------------------------------------------------------------------------------------------------------------------------
    ## Dashboard-Tab: rpe (Mietpreisschaetzung)
    ## ---------------------------------------------------------------------------------------------------------------------------------
    
    # Datensaetze vorbereiten und leere Value-Boxen und Residuendiagramme rendern.
    observe(priority = 100,{
      # Datensaetze fuer Dropdown selektieren (ausser Landkreis).
      updateSelectInput(session = session, inputId = "bundesland", choices = bundeslaender())
      updateSelectInput(session = session, inputId = "typeOfFlat", choices = typeOfFlat())
      updateSelectInput(session = session, inputId = "heatingType", choices = heatingType())
      
      # Value-Boxen mit dem Wert "0" rendern.
      output$vBoxPrice <- renderValueBox({ 
        valueBox(subtitle = "Geschätzter Mietpreis: ", value = paste0("0.00", "€"), icon = icon("building"), color = "green")
      })
      output$degreesOfFreedom <-renderValueBox({ 
        valueBox(subtitle = "Freiheitsgrade: ", value = paste0("0.000"), icon = icon("ruler-combined"), color = "green")
      })
      output$residualStdError <- renderValueBox({ 
        valueBox(subtitle = "Standardschätzfehler: ", value = paste0("0.000"), icon = icon("ruler-vertical"), color = "green")
      })
      output$rSquared <- renderValueBox({ 
        valueBox(subtitle = "R-Quadrat: ", value = paste0("0.000"), icon = icon("draw-polygon"), color = "green")
      })
      output$AdjRSquared <- renderValueBox({ 
        valueBox(subtitle = "Korrigiertes R-Quadrat: ", value = paste0("0.000"), icon = icon("vector-square"), color = "green")
      })
      output$fStatistic <- renderValueBox({ 
        valueBox(subtitle = "Teststatistik F-Wert: ", value = paste0("0.000"), icon = icon("vial"), color = "green")
      })
      #output$pValueFromFtest <- renderValueBox({ 
      #  valueBox(subtitle = "p-Wert des F-Tests", value = paste0("0.000"), icon = icon("vials"), color = "green")
      #})
      
      # Leere Residuendiagramme rendern.
      output$plotModellResiduals_1 <-renderPlot({
        plot(0)
      })
      output$plotModellResiduals_2 <-renderPlot({
        plot(0)
      })
      output$plotModellResiduals_3 <-renderPlot({
        plot(0)
      })
      output$plotModellResiduals_4 <-renderPlot({
        plot(0)
      })
    })
    
    # Parameter fuer die Mietpreisschaetzug rendern. Je nach dem, ob auf der Oberflaeche der Schalten gesetzt wurde oder nicht.
    observe(priority = 100, {
      # Togggles fuer die Parameter: Region.
      toggle(id = "bundesland",anim = TRUE,animType = "fade", condition = input$switchBL)
      toggle(id = "landkreis",anim = TRUE,animType = "fade", condition = input$switchLK)
      # Togggles fuer die Parameter: Wohnungsparameter.
      toggle(id = "typeOfFlat",anim = TRUE,animType = "fade", condition = input$switchTOF)
      toggle(id = "yearConstructed",anim = TRUE,animType = "fade", condition = input$switchYC)
      toggle(id = "heatingType",anim = TRUE,animType = "fade", condition = input$switchHT)
      toggle(id = "livingSpace",anim = TRUE,animType = "fade", condition = input$switchLS)
      toggle(id = "noRooms",anim = TRUE,animType = "fade", condition = input$switchNR)
      toggle(id = "floor",anim = TRUE,animType = "fade", condition = input$switchF)
      # Togggles fuer die Parameter: Ausstattung.
      toggle(id = "hasKitchen",anim = TRUE,animType = "fade", condition = input$switchHK)
      toggle(id = "cellar",anim = TRUE,animType = "fade", condition = input$switchC)
      toggle(id = "lift",anim = TRUE,animType = "fade", condition = input$switchL)
      toggle(id = "garden",anim = TRUE,animType = "fade", condition = input$switchG)
      toggle(id = "balcony",anim = TRUE,animType = "fade", condition = input$switchB)
    })
    
    # Drop-Down: Landkreis in Abhaengigkeiten des Bundeslandes selektieren.
    observe(priority = 100, {
      if(input$switchBL) {
        landkreiseSelected <- subset(landkreiseAll(), grepl(paste0('^',input$bundesland,'$'), landkreiseAll()$regio1),select = c("regio2"))
        updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseSelected$regio2)))
      }
      else {
        updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseAll()$regio2)))
      }
    })
    
    # Action-Button: "Mietpreis schaetzen" (getPrice).
    observeEvent(input$getPrice,{
      # Formel fuer die Regression erstellen.
      lmFormula <- createFormula(input,ignore = NA)
      
      # Datensatz mit Werten genieren die fuer die Vorhersage genutzt werden sollen.
      dfDatasetToPredict <- createFeatureDF(input)
      
      # Variable fuer die Datengrundlage leer intialisieren.
      data_immo_subset <- 0
      
      if(lmFormula == 0) { # Wenn kein Formel ermittelt wurde, dann muss der geschaetze Werte auf 0 gesetzt werden.
        predictEstate <- 0
      }
      else {
        withProgress(message = "Mietpreis wird geschätzt...", value = 0, min =0, max = 100, {
          incProgress(amount = 25, message = paste("Modell wird berechnet...")) # Ladebalkeninformation
          
          # Datengrundlage auf Basis des ausgewaehlten Bundeslandes und Langkreises festlegen.
          # Dementsprechend die Formel fuer die Regression anpassen.
          if(input$switchBL == TRUE && input$switchLK == TRUE) {
            data_immo_subset <- subset(data_immo(), grepl(paste0('^',input$bundesland,'$'),data_immo()$regio1) & grepl(paste0('^',input$landkreis,'$'),data_immo()$regio2), select = -c(regio1,regio2))
            lmFormula <- createFormula(input,c("regio1","regio2"))
          }
          else if(input$switchBL == TRUE && input$switchLK == FALSE) {
            data_immo_subset <- subset(data_immo(), grepl(paste0('^',input$bundesland,'$'),data_immo()$regio1), select = -c(regio1))
            lmFormula <- createFormula(input,c("regio1"))
          }
          else if(input$switchBL == FALSE && input$switchLK == TRUE) {
            data_immo_subset <- subset(data_immo(), grepl(paste0('^',input$landkreis,'$'),data_immo()$regio2), select = -c(regio2))
            lmFormula <- createFormula(input,c("regio2"))
          } 
          else {
            data_immo_subset <- data_immo()
            lmFormula <- createFormula(input, ignore = NA)
          }
          
          # Vorhersagemodell generieren und die Vorhersage treffen.
          fitEstate <- lm(data_immo_subset, formula = lmFormula)
          incProgress(amount = 50, message = paste("Mietpreis wird geschäzt...")) # Ladebalkeninformation
          predictEstate <- predict(object = fitEstate, dfDatasetToPredict)
          # DEBUG:
          #print(summary(fitEstate$signifi))
          #print(predictEstate)
          
          # Den geschaetzten Mietpreis in der Value-Box anzeigen.
          output$vBoxPrice <- renderValueBox({ 
            valueBox(subtitle = "Geschätzter Mietpreis: ", value = paste0(round(predictEstate,2), "€"), icon = icon("building"), color = "green")
          })
          
                    
          incProgress(amount = 75, message = paste("Angaben zum Modell werden ermittelt...")) # Ladebalkeninformation
          # Angaben zum Modell mit "summary" extrahieren.
          fitEstateSummary <- summary(fitEstate)
          
          # DEBUG:
          #print(fitEstateSummary)
          #print(paste("Freiheitsgrade: ", fitEstate$df.residual))
          #print(paste("Standardschätzfehler: ", round(fitEstateSummary$sigma,3)))
          #print(paste("R-Quadrat: ", round(fitEstateSummary$r.squared,3)))
          #print(paste("Korrigiertes R-Quadrat: ", round(fitEstateSummary$adj.r.squared,3)))
          #print(paste("Teststatistik F-Wert: ", round(fitEstateSummary$fstatistic[1],3)))
          #print(paste("p-Wert: ",round(pf(fitEstateSummary$fstatistic[1],fitEstateSummary$fstatistic[2],fitEstateSummary$fstatistic[3],lower.tail = FALSE),3)))
          
          
          # Aus den zusammengefassten Daten zum Modell die einzelnen Modell angaben in den Value-Boxen anzeigen.
          output$degreesOfFreedom <-renderValueBox({ 
            valueBox(subtitle = "Freiheitsgrade: ", value = paste0(fitEstate$df.residual), icon = icon("ruler-combined"), color = "green")
          })
          output$residualStdError <- renderValueBox({ 
            valueBox(subtitle = "Standardschätzfehler: ", value = paste0(round(fitEstateSummary$sigma,3)), icon = icon("ruler-vertical"), color = "green")
          })
          output$rSquared <- renderValueBox({ 
            valueBox(subtitle = "R-Quadrat: ", value = paste0(round(fitEstateSummary$r.squared,3)), icon = icon("draw-polygon"), color = "green")
          })
          output$AdjRSquared <- renderValueBox({ 
            valueBox(subtitle = "Korrigiertes R-Quadrat: ", value = paste0(round(fitEstateSummary$adj.r.squared,3)), icon = icon("vector-square"), color = "green")
          })
          output$fStatistic <- renderValueBox({ 
            valueBox(subtitle = "Teststatistik F-Wert: ", value = paste0(round(fitEstateSummary$fstatistic[1],3)), icon = icon("vial"), color = "green")
          })
          #output$pValueFromFtest <- renderValueBox({ 
          #  valueBox(subtitle = "p-Wert des F-Tests", value = paste0(round(pf(fitEstateSummary$fstatistic[1],fitEstateSummary$fstatistic[2],fitEstateSummary$fstatistic[3],lower.tail = FALSE),3)), icon = icon("vials"), color = "green")
          #})
          
          incProgress(amount = 75, message = paste("Residuendiagramme werden generiert...")) # Ladebalkeninformation
          
          # Residuendiagramme aus dem Modell rendern.
          output$plotModellResiduals_1 <-renderPlot({
            plot(fitEstate,which= 1)
          })
          output$plotModellResiduals_2 <-renderPlot({
            plot(fitEstate,which= 2)
          })
          output$plotModellResiduals_3 <-renderPlot({
            plot(fitEstate,which= 3)
          })
          output$plotModellResiduals_4 <-renderPlot({
            plot(fitEstate,which= 4)
          })
 
        })
      }
    })
    
    ## ---------------------------------------------------------------------------------------------------------------------------------
    ## Dashboard-Tab: baseData (Datengrundlage)
    ## ---------------------------------------------------------------------------------------------------------------------------------
    
    # Datengrundlage als Tabelle rendern.
    # Uebersetzung einiger Tabellenparameter in Deutsch.
    # Uebersetzung der technischen Spaltennamen.
    output$data_table <- DT::renderDataTable(
      datatable(exploreImmoDataTable, 
        options = list(
          scrollX = TRUE,
          language = list(
            info = '_START_ bis _END_ von _TOTAL_ Datensätzen',
            paginate = list(previous = 'Zurück', `next` = 'Vor'),
            lengthMenu = '_MENU_ Einträge pro Seite',
            search = 'Suche:',
            thousands = '.',
            zeroRecords = 'Keine Daten gefunden'
          )
        ), 
        filter = 'top',  class = 'table-bordered table-striped',  rownames = FALSE, selection = "none", style = 'bootstrap4',
        colnames = c("Bundesland","Nebenkosten","Heizungsart","Balkon","Baujahr","Küche","Keller","Kaltmiete","Wohnfläche","Aufzug","Wohnungstyp","PLZ","Zimmeranzahl","Stockwerk","Garten","Landkreis")
      ) %>% formatCurrency(c(2,8), '\U20AC', digits = 2, before = FALSE) %>% formatCurrency(9, 'qm', digits = 2, before = FALSE) 
    ) 

})

## ---------------------------------------------------------------------------------------------------------------------------------
## Hilfsfunktionen
## ---------------------------------------------------------------------------------------------------------------------------------

# Datensatz mit Werten genieren die fuer die Vorhersage genutzt werden sollen.
createFeatureDF <- function(input) {
  df_immo_empty <- df_immo_cleaned[1,]
  df_immo_empty[1,] <- NA
  df_immo_empty[1,] <- list(
    regio1 = input$bundesland,
    serviceCharge = NA,
    heatingType = input$heatingType,
    balcony = input$balcony,
    yearConstructed = input$yearConstructed,
    hasKitchen = input$hasKitchen,
    cellar = input$cellar,
    baseRent = NA,
    livingSpace = input$livingSpace,
    lift = input$lift,
    typeOfFlat = input$typeOfFlat,
    geo_plz = NA,
    noRooms = input$noRooms,
    floor = input$floor,
    garden = input$garden,
    regio2 = input$landkreis
    )
  return(df_immo_empty)
}

# Formel fuer die Regression erstellen.
createFormula <- function(input,ignore) {
  formula <- "baseRent ~"
  usedFieldCount <- 0
  fieldList <- list(list(field='regio1', value = input$switchBL, sep = "+"),
            list(field='regio2', value = input$switchLK, sep = "+"),
            list(field='typeOfFlat', value = input$switchTOF, sep = "+"),
            list(field='heatingType', value = input$switchHT, sep = "+"),
            list(field='yearConstructed', value = input$switchYC, sep = "+"),
            list(field='livingSpace', value = input$switchLS, sep = "+"),
            list(field='noRooms', value = input$switchNR, sep = "+"),
            list(field='floor', value = input$switchF, sep = "+"),
            list(field='hasKitchen', value = input$switchHK, sep = "+"),
            list(field='cellar', value = input$switchC, sep = "+"),
            list(field='lift', value = input$switchL, sep = "+"),
            list(field='garden', value = input$switchG, sep = "+"),
            list(field='balcony', value = input$switchB, sep = "+")
  )
  for(fl in fieldList) {
    if(fl$value && !(fl$field %in% ignore)) {
      usedFieldCount <- usedFieldCount + 1
      if(usedFieldCount == 1) {
        formula <- str_c(formula, fl$field, sep = " ")
      } else {
        formula <- str_c(formula, fl$field, sep = fl$sep)
      }
    }
  }
  
  if(usedFieldCount == 0) {
    formula <- 0
  }
  # DEBUG: 
  #print(formula)
  return(formula)
}

# Desktriptive Statistken berechnen.
computeDashboardStatistics <-function(immo_data, fields) {
  
  if(fields == "all" || fields == "baseRent") {
    dfSummaryBaseRent <- list(
      median = round(median(immo_data$baseRent),2),
      avg = round(mean(immo_data$baseRent),2),
      min = round(min(immo_data$baseRent),2),
      max = round(max(immo_data$baseRent),2),
      q25 = round(unname(quantile(immo_data$baseRent, probs = 0.25),2)),
      q75 = round(unname(quantile(immo_data$baseRent, probs = 0.75),2))
    )
  }
  if(fields == "all" || fields == "livingSpace") {
    dfSummaryLivingSpace <- list(
      median = round(median(immo_data$livingSpace),2),
      avg = round(mean(immo_data$livingSpace),2),
      min = round(min(immo_data$livingSpace),2),
      max = round(max(immo_data$livingSpace),2),
      q25 = round(unname(quantile(immo_data$livingSpace, probs = 0.25),2)),
      q75 = round(unname(quantile(immo_data$livingSpace, probs = 0.75),2))
    )
  }
  if(fields == "all" || fields == "livingSpace") {
    dfSummaryNoRooms <- list(
      median = round(median(immo_data$noRooms),2),
      avg = round(mean(immo_data$noRooms),2),
      min = round(min(immo_data$noRooms),2),
      max = round(max(immo_data$noRooms),2),
      q25 = round(unname(quantile(immo_data$noRooms, probs = 0.25),2)),
      q75 = round(unname(quantile(immo_data$noRooms, probs = 0.75),2))
    )
  }
  
  return(rbind(dfSummaryBaseRent, dfSummaryLivingSpace, dfSummaryNoRooms))
}


