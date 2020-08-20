
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(ggplot2)
library(tidyverse)

# Helper functions

# Define UI for application that draws a histogram
shinyUI(
    
    dashboardPage(
        
        dashboardHeader(title = "German real estates",titleWidth = 300),
        dashboardSidebar(width = 300,
                         sidebarMenu(id = "tabs",
                                     menuItem("Dash Board", tabName = "dashBoard", icon = icon("tachometer-alt")),
                                     menuItem("Datengrundlage", tabName = "baseData", icon = icon("table")),
                                     menuItem("Mietpreisschätzung", tabName = "rpe", icon = icon("chart-line"))
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
                                        fluidRow(
                                            column(12,
                                                column(3,
                                                       prettySwitch(inputId = "switchBL",label = "Bundesland:",value = TRUE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                ),
                                                column(9,
                                                       selectInput("bundesland", label = NULL, choices = NULL)
                                                )
                                            )  
                                        ),
                                        fluidRow(
                                            column(12,
                                                   column(3,
                                                        prettySwitch(inputId = "switchLK",label = "Landkreis:",value = TRUE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(9,
                                                        selectInput("landkreis", label = NULL, choices = NULL)
                                                   )
                                            )  
                                        )
                                    ),
                                    box(width=NULL, solidHeader = TRUE, status = "warning", title = "Wohnungsparameter:", collapsible = TRUE, collapsed = FALSE,
                                        fluidRow(
                                            column(6,
                                                   column(7,
                                                          prettySwitch(inputId = "switchYC",label = "Baujahr:",value = TRUE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(5,
                                                          numericInput("yearConstructed", label = NULL, min = 1215, max = 2020, value = 2010)
                                                   )
                                            ),
                                            column(6,
                                                   column(7,
                                                          prettySwitch(inputId = "switchLS",label = "Wohnfläche in qm:",value = TRUE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(5,
                                                          numericInput("livingSpace", label = NULL, min = 1, value = 50)
                                                   )
                                            )
                                        ),
                                        fluidRow(
                                            column(6,
                                                   column(7,
                                                          prettySwitch(inputId = "switchNR",label = "Zimmeranzahl:",value = TRUE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(5,
                                                          numericInput("noRooms", label = NULL, min = 1, value = 2)
                                                   )
                                            ),
                                            column(6,
                                                   column(7,
                                                          prettySwitch(inputId = "switchF",label = "Stockwerk:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(5,
                                                          numericInput("floor", label = NULL, min = -1, value = 1)
                                                   )
                                            )
                                        ),
                                        fluidRow(
                                            column(12,
                                                   column(3,
                                                          prettySwitch(inputId = "switchTOF",label = "Typ:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(9,
                                                          selectInput("typeOfFlat", label = NULL, choices = NULL)
                                                   )
                                            )
                                        ),
                                        fluidRow(
                                            column(12,
                                                   column(3,
                                                          prettySwitch(inputId = "switchHT",label = "Heizungsart:",value = FALSE, bigger = FALSE, slim = FALSE, width = "100%",fill = TRUE, status = "warning", inline = TRUE)
                                                   ),
                                                   column(9,
                                                          selectInput("heatingType", label = NULL, choices = NULL)
                                                   )
                                            )
                                        ),
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
                            box(width=NULL, solidHeader = TRUE, status = "primary", title = "(2) Ergebnisse der Mietpreisschätzung:",
                                fluidRow(
                                    column(12,
                                           valueBoxOutput("vBoxPrice", width = NULL)       
                                    )
                                ),
                                fluidRow(
                                    column(12,
                                        box(width = NULL, solidHeader = TRUE, status = "warning", title = "Angaben zum Modell:",collapsible = TRUE, collapsed = TRUE,
                                            fluidRow(
                                                column(4,valueBoxOutput("degreesOfFreedom", width = NULL)),
                                                column(4,valueBoxOutput("rSquared", width = NULL)),
                                                column(4,valueBoxOutput("fStatistic", width = NULL))
                                            ),
                                            fluidRow(
                                                column(4,valueBoxOutput("residualStdError", width = NULL)),
                                                column(4,valueBoxOutput("AdjRSquared", width = NULL)),
                                                column(4,valueBoxOutput("pValueFromFtest", width = NULL))
                                            )
                                        )
                                    )  
                                ),
                                fluidRow(
                                    column(12,
                                        box(width = NULL, solidHeader = TRUE, status = "warning", title = "Residuendiagramme:",collapsible = TRUE, collapsed = TRUE,
                                            fluidRow(column(12,plotOutput("plotModellResiduals_1"))),    
                                            fluidRow(column(12,plotOutput("plotModellResiduals_2"))),
                                            fluidRow(column(12,plotOutput("plotModellResiduals_3"))),
                                            fluidRow(column(12,plotOutput("plotModellResiduals_4")))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
