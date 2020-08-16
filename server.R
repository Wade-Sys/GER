library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ## Tab: rpe
    ## ---------------------------------------------------------------------------------------------------------------------------------
    # Create reactive datasets
    data_immo <- reactive(df_immo_cleaned)
    bundeslaender <- reactive(unique(sort(data_immo()$regio1)))
    landkreiseAll <- reactive(unique(data_immo()[c('regio1','regio2')]))
    typeOfFlat <- reactive(unique(sort(data_immo()$typeOfFlat)))
    heatingType <- reactive(unique(sort(data_immo()$heatingType)))
    
    
    # Observe initial dataset
    observe(priority = 100,{
      updateSelectInput(session = session, inputId = "bundesland", choices = bundeslaender())
      updateSelectInput(session = session, inputId = "typeOfFlat", choices = typeOfFlat())
      updateSelectInput(session = session, inputId = "heatingType", choices = heatingType())
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
        print("NO")
      }
      else {
        withProgress(message = "Mietpreis wird geschätzt...", value = 0, min =0, max = 100, {
          incProgress(amount = 25, message = paste("Modell wird erstellt..."))
          fitEstate <- lm(data_immo(), formula = lmFormula)
          incProgress(amount = 50, message = paste("Mietpreis wird geschäzt..."))
          predictEstate <- predict(object = fitEstate, dfDatasetToPredict)
        
          #print(summary(fitEstate))
          print(predictEstate)
          incProgress(amount = 75, message = paste("Ausgabe..."))
          output$vBoxPrice <- renderValueBox({
            valueBox(
              subtitle = "Geschätzter Mietpreis: ", value = paste0(round(predictEstate,2), "€"), icon = icon("building"), color = "green"
            )
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



