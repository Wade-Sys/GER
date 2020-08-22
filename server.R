
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(broom)
#library(car)

#df_immo_cleaned <- read_csv2(file = "immo_scout_cleaned_final.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # Create reactive datasets
    data_immo <- reactive(df_immo_cleaned)
    bundeslaender <- reactive(unique(sort(data_immo()$regio1)))
    landkreiseAll <- reactive(unique(data_immo()[c('regio1','regio2')]))
    typeOfFlat <- reactive(unique(sort(data_immo()$typeOfFlat)))
    heatingType <- reactive(unique(sort(data_immo()$heatingType)))
    data_immo_subset_db <- reactive(0)
    immoDashboardStatistics <- reactive(0)
    
    ## Tab: dashboard
    ## ---------------------------------------------------------------------------------------------------------------------------------
    
    # Observe Bundesland
    observe(priority = 100, {
      updateSelectInput(session = session, inputId = "dbBundesland", choices = c("Alle",bundeslaender()))
    })
    
    # Observe Landkreis
    observe(priority = 100, {
      if(input$dbBundesland == 'Alle') {
        dbLkSelected <- subset(landkreiseAll(), select = c("regio2"))
        
      } else {
        dbLkSelected <- subset(landkreiseAll(), grepl(input$dbBundesland, landkreiseAll()$regio1),select = c("regio2"))
      }
      
      updateSelectInput(session = session, inputId = "dbLandkreis", choices = c("Alle",sort(unique(dbLkSelected$regio2))))
      
    })
    
    # Observe values
    observe(priority = 100, {
      
      # Subset data by Bundesland and Landkreis for computation
      if(input$dbBundesland == "Alle" && input$dbLandkreis == "Alle") {
        data_immo_subset_db <- data_immo()
      }
      else if(input$dbBundesland == "Alle" && input$dbLandkreis != "Alle") {
        data_immo_subset_db <- subset(data_immo(), grepl(input$dbLandkreis, data_immo()$regio2))
      }
      else if(input$dbBundesland != "Alle" && input$dbLandkreis == "Alle") {
        data_immo_subset_db <- subset(data_immo(), grepl(input$dbBundesland, data_immo()$regio1))
      }
      else if(input$dbBundesland != "Alle" && input$dbLandkreis != "Alle") {
         data_immo_subset_db <- subset(data_immo(), grepl(input$dbBundesland, data_immo()$regio1) & grepl(input$dbLandkreis, data_immo()$regio2))
      }
      
      immoDashboardStatistics <- reactive(computeDashboardStatistics(data_immo_subset_db))
      print(immoDashboardStatistics())
      
    })
    
    ## Tab: rpe
    ## ---------------------------------------------------------------------------------------------------------------------------------
    # Observe initial dataset
    observe(priority = 100,{
      updateSelectInput(session = session, inputId = "bundesland", choices = bundeslaender())
      updateSelectInput(session = session, inputId = "typeOfFlat", choices = typeOfFlat())
      updateSelectInput(session = session, inputId = "heatingType", choices = heatingType())
      
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
      output$pValueFromFtest <- renderValueBox({ 
        valueBox(subtitle = "p-Wert des F-Tests", value = paste0("0.000"), icon = icon("vials"), color = "green")
      })
      
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
    
    # Observe toggles
    observe(priority = 100, {
      # Toggle Region
      toggle(id = "bundesland",anim = TRUE,animType = "fade", condition = input$switchBL)
      toggle(id = "landkreis",anim = TRUE,animType = "fade", condition = input$switchLK)
      # Toggle Wohnungsparameter
      toggle(id = "typeOfFlat",anim = TRUE,animType = "fade", condition = input$switchTOF)
      toggle(id = "yearConstructed",anim = TRUE,animType = "fade", condition = input$switchYC)
      toggle(id = "heatingType",anim = TRUE,animType = "fade", condition = input$switchHT)
      toggle(id = "livingSpace",anim = TRUE,animType = "fade", condition = input$switchLS)
      toggle(id = "noRooms",anim = TRUE,animType = "fade", condition = input$switchNR)
      toggle(id = "floor",anim = TRUE,animType = "fade", condition = input$switchF)
      # Toggle Ausstattung
      toggle(id = "hasKitchen",anim = TRUE,animType = "fade", condition = input$switchHK)
      toggle(id = "cellar",anim = TRUE,animType = "fade", condition = input$switchC)
      toggle(id = "lift",anim = TRUE,animType = "fade", condition = input$switchL)
      toggle(id = "garden",anim = TRUE,animType = "fade", condition = input$switchG)
      toggle(id = "balcony",anim = TRUE,animType = "fade", condition = input$switchB)
    })
    
    # Observe Landkreise
    observe(priority = 100, {
      # Filter dataset for landkreise
      if(input$switchBL) {
        landkreiseSelected <- subset(landkreiseAll(), grepl(input$bundesland, landkreiseAll()$regio1),select = c("regio2"))
        updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseSelected$regio2)))
      }
      else {
        updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseAll()$regio2)))
      }
    })
    
    ## ObserveEvent: getPrice
    observeEvent(input$getPrice,{
      lmFormula <- createFormula(input)
      dfDatasetToPredict <- createFeatureDF(input)
      if(lmFormula == 0) {
        predictEstate <- 0
      }
      else {
        withProgress(message = "Mietpreis wird geschätzt...", value = 0, min =0, max = 100, {
          incProgress(amount = 25, message = paste("Modell wird berechnet..."))
          fitEstate <- lm(data_immo(), formula = lmFormula)
          incProgress(amount = 50, message = paste("Mietpreis wird geschäzt..."))
          predictEstate <- predict(object = fitEstate, dfDatasetToPredict)
        
          #print(summary(fitEstate$signifi))
          #print(predictEstate)
          output$vBoxPrice <- renderValueBox({ 
            valueBox(subtitle = "Geschätzter Mietpreis: ", value = paste0(round(predictEstate,2), "€"), icon = icon("building"), color = "green")
          })
          
          incProgress(amount = 75, message = paste("Angaben zum Modell werden ermittelt..."))
          
          fitEstateSummary <- summary(fitEstate)
          print(fitEstateSummary)
          #print(paste("Freiheitsgrade: ", fitEstate$df.residual))
          #print(paste("Standardschätzfehler: ", round(fitEstateSummary$sigma,3)))
          #print(paste("R-Quadrat: ", round(fitEstateSummary$r.squared,3)))
          #print(paste("Korrigiertes R-Quadrat: ", round(fitEstateSummary$adj.r.squared,3)))
          #print(paste("Teststatistik F-Wert: ", round(fitEstateSummary$fstatistic[1],3)))
          #print(paste("p-Wert: ",round(pf(fitEstateSummary$fstatistic[1],fitEstateSummary$fstatistic[2],fitEstateSummary$fstatistic[3],lower.tail = FALSE),3)))
          
          
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
          output$pValueFromFtest <- renderValueBox({ 
            valueBox(subtitle = "p-Wert des F-Tests", value = paste0(round(pf(fitEstateSummary$fstatistic[1],fitEstateSummary$fstatistic[2],fitEstateSummary$fstatistic[3],lower.tail = FALSE),3)), icon = icon("vials"), color = "green")
          })
          
          incProgress(amount = 75, message = paste("Residuendiagramme werden generiert..."))
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
    ## Tab: baseData
    # Datetable for data overview
    output$data_table <- DT::renderDataTable(data_immo(), options = list(scrollX = TRUE),filter = 'top', server = TRUE)

})
###### Helper functions
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
    regio2 = input$landkreis,
    regio3 = NA,
    date = NA
    )
  return(df_immo_empty)
}

createFormula <- function(input) {
  formula <- "baseRent ~"
  usedFieldCount <- 0
  fieldList <- list(list(field='regio1', value = input$switchBL),
            list(field='regio2', value = input$switchLK),
            list(field='typeOfFlat', value = input$switchTOF),
            list(field='heatingType', value = input$switchHT),
            list(field='yearConstructed', value = input$switchYC),
            list(field='livingSpace', value = input$switchLS),
            list(field='noRooms', value = input$switchNR),
            list(field='floor', value = input$switchF),
            list(field='hasKitchen', value = input$switchHK),
            list(field='cellar', value = input$switchC),
            list(field='lift', value = input$switchL),
            list(field='garden', value = input$switchG),
            list(field='balcony', value = input$switchB)
  )
  for(fl in fieldList) {
    if(fl$value) {
      usedFieldCount <- usedFieldCount + 1
      if(usedFieldCount == 1) {
        formula <- str_c(formula, fl$field, sep = " ")
      } else {
        formula <- str_c(formula, fl$field, sep = " + ")
      }
    }
  }
  
  if(usedFieldCount == 0) {
    formula <- 0
  }
  
  return(formula)
}

computeDashboardStatistics <-function(immo_data) {
  
  dfSummaryBaseRent <- list(
    median = median(immo_data$baseRent),
    avg = mean(immo_data$baseRent),
    min = min(immo_data$baseRent),
    max = max(immo_data$baseRent),
    q25 = quantile(immo_data$baseRent, probs = 0.25),
    q75 = quantile(immo_data$baseRent, probs = 0.75)
  )
  
  dfSummaryLivingSpace <- list(
    median = median(immo_data$livingSpace),
    avg = mean(immo_data$livingSpace),
    min = min(immo_data$livingSpace),
    max = max(immo_data$livingSpace),
    q25 = quantile(immo_data$livingSpace, probs = 0.25),
    q75 = quantile(immo_data$livingSpace, probs = 0.75)
  )
  
  dfSummaryNoRooms <- list(
    median = median(immo_data$noRooms),
    avg = mean(immo_data$noRooms),
    min = min(immo_data$noRooms),
    max = max(immo_data$noRooms),
    q25 = quantile(immo_data$noRooms, probs = 0.25),
    q75 = quantile(immo_data$noRooms, probs = 0.75)
  )

  
  return(rbind(dfSummaryBaseRent, dfSummaryLivingSpace, dfSummaryNoRooms))
}


