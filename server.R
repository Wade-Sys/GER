library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
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
    
    # Datetable for data overview
    output$data_table <- DT::renderDataTable(data_immo(), options = list(scrollX = TRUE),filter = 'top', server = TRUE)

})
###### Helper functions


