library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    data_immo <- reactive(df_immo_cleaned)
    bundeslaender <- reactive(unique(sort(data_immo()$regio1)))
    landkreiseAll <- reactive(unique(data_immo()[c('regio1','regio2')]))
    typeOfFlat <- reactive(unique(sort(data_immo()$typeOfFlat)))
    heatingType <- reactive(unique(sort(data_immo()$heatingType)))
    
    observe(priority = 100,{
      #print(landkreiseAll())
      updateSelectInput(session = session, inputId = "bundesland", choices = bundeslaender())
      # Filter dataset for landkreise
      #if(input$switchBL) {
      #  landkreiseSelected <- subset(landkreiseAll(), grepl(input$bundesland, landkreiseAll()$regio2),select = c("regio2"))
      #  print(landkreiseSelected)
      #  updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseSelected$regio2)))
      #}
      #else {
      #  updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseAll()$regio2)))
      #}
      
      landkreiseSelected <- subset(landkreiseAll(), (grepl(input$bundesland, landkreiseAll()$regio2) & input$switchBL == TRUE),select = c("regio2"))
      updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(landkreiseSelected$regio2)))
      
      updateSelectInput(session = session, inputId = "typeOfFlat", choices = typeOfFlat())
      updateSelectInput(session = session, inputId = "heatingType", choices = heatingType())
      
      # Toggle Region
      toggle(id = "bundesland",anim = TRUE,animType = "slide", condition = input$switchBL)
      toggle(id = "landkreis",anim = TRUE,animType = "slide", condition = input$switchLK)
      # Toggle Wohnungsparameter
      toggle(id = "typeOfFlat",anim = TRUE,animType = "slide", condition = input$switchTOF)
      toggle(id = "yearConstructed",anim = TRUE,animType = "slide", condition = input$switchYC)
      toggle(id = "heatingType",anim = TRUE,animType = "slide", condition = input$switchHT)
      toggle(id = "livingSpace",anim = TRUE,animType = "slide", condition = input$switchLS)
      toggle(id = "noRooms",anim = TRUE,animType = "slide", condition = input$switchNR)
      toggle(id = "floor",anim = TRUE,animType = "slide", condition = input$switchF)
      # Toggle Ausstattung
      toggle(id = "hasKitchen",anim = TRUE,animType = "fade", condition = input$switchHK)
      toggle(id = "cellar",anim = TRUE,animType = "fade", condition = input$switchC)
      toggle(id = "lift",anim = TRUE,animType = "fade", condition = input$switchL)
      toggle(id = "garden",anim = TRUE,animType = "fade", condition = input$switchG)
      toggle(id = "balcony",anim = TRUE,animType = "fade", condition = input$switchB)

    })
    
    
    
    #observe(priority = 100,{
    #  data_immo_subset <- subset(data_immo(),grepl(input$bundesland, bundeslaender), select = c("regio1","regio2","typeOfFlat","heatingType"))
    #  updateSelectInput(session = session, inputId = "landkreis", choices = sort(unique(data_immo_subset$regio2)))
    #  updateSelectInput(session = session, inputId = "typeOfFlat", choices = sort(unique(data_immo_subset$typeOfFlat)))
    #  updateSelectInput(session = session, inputId = "heatingType", choices = sort(unique(data_immo_subset$heatingType)))
    #})
    # observe({
    #     data_immo_subset <- subset(data_immo(),grepl(input$landkreis, data_immo()$regio2), select = c("regio1","regio2","typeOfFlat","heatingType"))
    #     updateSelectInput(session = session, inputId = "typeOfFlat", choices = sort(unique(data_immo_subset$typeOfFlat)))
    #     updateSelectInput(session = session, inputId = "heatingType", choices = sort(unique(data_immo_subset$heatingType)))
    # },priority = 9)
    # 
    # observeEvent(input$getPrice, {
    #     dataSelection <- c(regio1=input$bundesland, 
    #                     regio2=input$landkreis, 
    #                     typeOfFlat=input$typeOfFlat, 
    #                     heatingType=input$heatingType, 
    #                     hasKitchen=input$hasKitchen, 
    #                     cellar=input$cellar, 
    #                     lift=input$lift, 
    #                     garden=input$garden, 
    #                     balcony=input$balcony,
    #                     noRooms=input$noRooms,
    #                     floor=input$floor,
    #                     livingSpace=input$livingSpace,
    #                     yearConstructed=input$yearConstructed
    #                     )
    #     print(dataSelection)
    #     data_immo_selection <- subset(data_immo(),grepl(input$landkreis, data_immo()$regio2), select = c("baseRent",))
    # })
  
    
    
    
    output$data_table <- DT::renderDataTable(data_immo(), options = list(scrollX = TRUE),filter = 'top', server = TRUE)

})
  

