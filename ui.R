
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)

# Helper functions

# Define UI for application that draws a histogram
shinyUI(
    
    dashboardPage(
        
        dashboardHeader(title = "German real estates",titleWidth = 300),
        dashboardSidebar(width = 300,
                         sidebarMenu(id = "tabs",
                                     menuItem("Dash Board", tabName = "dashBoard", icon = icon("clipboard")),
                                     menuItem("Datengrundlage", tabName = "baseData", icon = icon("table")),
                                     menuItem("Mietpreisschätzung", tabName = "rpe", icon = icon("building"))
                         )
        ),
        dashboardBody(
            useShinyjs(),
            tabItems(
                tabItem(tabName = "dashBoard",
                    fluidRow(
                        box(title = "Durchschnittsmiete pro Bundesland und Datum", "TODO"
                        )        
                    )
                ),
                tabItem(tabName = "baseData",
                        fluidRow(column(12, dataTableOutput("data_table")))
                ),
                tabItem(tabName = "rpe",
                    fluidRow(
                        column(width=5,
                            box(width=NULL, solidHeader = TRUE, status = "primary", title = "(1) Welche Parameter sollen bei der Schätzung berücksichtigt werden?",
                                column(12,
                                    box(width=NULL, solidHeader = TRUE, status = "warning", title = "Region:",collapsible = TRUE, collapsed = FALSE,
                                        prettySwitch(inputId = "switchBL",label = "Bundesland:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        selectInput("bundesland", label = NULL, choices = NULL),
                                        prettySwitch(inputId = "switchLK",label = "Landkreis:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        selectInput("landkreis", label = NULL, choices = NULL)
                                    ),
                                    box(width=NULL, solidHeader = TRUE, status = "warning", title = "Wohnungsparameter:", collapsible = TRUE, collapsed = FALSE,
                                        prettySwitch(inputId = "switchTOF",label = "Typ:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        selectInput("typeOfFlat", label = NULL, choices = NULL),
                                        prettySwitch(inputId = "switchHT",label = "Heizungsart:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        selectInput("heatingType", label = NULL, choices = NULL),
                                        prettySwitch(inputId = "switchYC",label = "Baujahr:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        selectInput("yearConstructed", label = NULL, sort(seq(1215,2020))),
                                        prettySwitch(inputId = "switchLS",label = "Wohnfläche in qm:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        numericInput("livingSpace", label = NULL, min = 1, value = 50),
                                        prettySwitch(inputId = "switchNR",label = "Zimmeranzahl:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        numericInput("noRooms", label = NULL, min = 1, value = 2),
                                        prettySwitch(inputId = "switchF",label = "Stockwerk:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                        numericInput("floor", label = NULL, min = -1, value = 1)
                                    ),
                                    box(width=NULL, solidHeader = TRUE, status = "warning", title = "Ausstattung", collapsible = TRUE, collapsed = FALSE,
                                        fluidRow(
                                            column(6,
                                                column(5,
                                                    prettySwitch(inputId = "switchHK",label = "Küche:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                                ),
                                                column(1,
                                                    prettyToggle(inputId = "hasKitchen",label_on = "vorhanden", label_off = "nicht vorhanden",value = TRUE,inline = TRUE),
                                                ),
                                            ),
                                            column(6,
                                                column(5,
                                                    prettySwitch(inputId = "switchC",label = "Keller:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                                ),
                                                column(2,
                                                    prettyToggle(inputId = "cellar",label_on = "vorhanden", label_off = "nicht vorhanden",value = TRUE,inline = TRUE)
                                                )
                                            )
                                        ),
                                        fluidRow(
                                            column(6,
                                                column(5,
                                                       prettySwitch(inputId = "switchL",label = "Aufzug:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                                ),
                                                column(1,
                                                       prettyToggle(inputId = "lift",label_on = "vorhanden", label_off = "nicht vorhanden",value = TRUE,inline = TRUE),
                                                )
                                            ),
                                            column(6,
                                                   column(5,
                                                          prettySwitch(inputId = "switchG",label = "Garten:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                                   ),
                                                   column(1,
                                                          prettyToggle(inputId = "garden",label_on = "vorhanden", label_off = "nicht vorhanden",value = TRUE,inline = TRUE),
                                                   )
                                            )
                                        ),
                                        fluidRow(
                                            column(6,
                                                column(5,
                                                       prettySwitch(inputId = "switchB",label = "Balkon:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE),
                                                ),
                                                column(1,
                                                       prettyToggle(inputId = "balcony",label_on = "vorhanden", label_off = "nicht vorhanden",value = TRUE,inline = TRUE)
                                                )
                                            )
                                        )
                                    ),
                                    actionButton("getPrice", "Mietpreis schätzen", width = "100%", class="btn btn-info")
                                )
                            )
                        ),
                        column(7,
                            box(width=NULL, solidHeader = TRUE, status = "primary", title = "(2) Ergebnisse der Mietpreisschätzung:")
                        )
                    )
                )
            )
        )
    )
)
