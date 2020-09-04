# Bibliotheken laden.
library(install.load)
install_load(
    "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "dashboardthemes", "DT", "ggplot2", "tidyverse", "broom", "maps", "mapdata", "rgdal", "sp", "mapproj"
)
#library(shiny)
#library(shinydashboard)
#library(shinyWidgets)
#library(shinyjs)
#library(dashboardthemes)
#library(DT)
#library(ggplot2)
#library(tidyverse)
#library(broom)
#library(maps)
#library(mapdata)
#library(rgdal)
#library(sp)
#library(mapproj)


# UI-Logik
# Das Layout der UI basiert auf Shinydashboard
# Desweiteren wurden fuer zusaetzliche Elemente "shinyWidgets" verwenden.
# Fuer die Tabellen wird "DT" verwendet.
# Das Theme kommt aus der Library "dashboardthemes"
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
             shinyDashboardThemes(
                 theme = "blue_gradient"
             ),
            tabItems(
                ## ---------------------------------------------------------------------------------------------------------------------------------
                ## Dashboard-Tab: dashboard
                ## ---------------------------------------------------------------------------------------------------------------------------------
                tabItem(tabName = "dashBoard",
                    fluidRow(
                        #### Linke Box: Statistische Kennzahlen
                        column(8,
                            #### Tab-Panel
                            tabBox(id = "tabBoxSK", title = tagList(tags$span(icon("chart-pie"),"Statistische Kennzahlen:",style="font-size:28px")), width = NULL, height = "100%",
                                #### Tab: Kaltmiete
                                tabPanel(tagList(tags$span("Kaltmiete:",style="font-size:24px")), 
                                    fluidRow(
                                        infoBoxOutput("dbInfoBoxBaseRentAvg"),
                                        infoBoxOutput("dbInfoBoxBaseRentMedian"),
                                        infoBoxOutput("dbInfoBoxBaseRentMin"),
                                        infoBoxOutput("dbInfoBoxBaseRentMax"),
                                        infoBoxOutput("dbInfoBoxBaseRentQ25"),
                                        infoBoxOutput("dbInfoBoxBaseRentQ75")
                                    ),
                                    fluidRow(
                                        column(12,
                                            box(width = NULL, solidHeader = TRUE, status = "primary", title = tagList(tags$span("Verteilung der Daten im Boxplot:",style="font-size:22px")),
                                                plotOutput("dbBaseRentBoxplot")
                                            )
                                        )
                                    )
                                ),
                                #### Tab: Wohnflaeche
                                tabPanel(tagList(tags$span("Wohnfläche:",style="font-size:24px")),
                                    fluidRow(
                                        infoBoxOutput("dbInfoBoxLivingSpaceAvg"),
                                        infoBoxOutput("dbInfoBoxLivingSpaceMedian"),
                                        infoBoxOutput("dbInfoBoxLivingSpaceMin"),
                                        infoBoxOutput("dbInfoBoxLivingSpaceMax"),
                                        infoBoxOutput("dbInfoBoxLivingSpaceQ25"),
                                        infoBoxOutput("dbInfoBoxLivingSpaceQ75")
                                    ),
                                    fluidRow(
                                        column(12,
                                            box(width = NULL, solidHeader = TRUE, status = "primary", title = tagList(tags$span("Verteilung der Daten im Boxplot:",style="font-size:22px")),
                                                plotOutput("dbLivingSpaceBoxplot")
                                            )
                                        )
                                    )
                                ),
                                #### Tab: Anzahl Zimmer
                                tabPanel(tagList(tags$span("Anzahl Zimmer:",style="font-size:24px")),
                                     fluidRow(
                                         infoBoxOutput("dbInfoBoxNoRoomsAvg"),
                                         infoBoxOutput("dbInfoBoxNoRoomsMedian"),
                                         infoBoxOutput("dbInfoBoxNoRoomsMin"),
                                         infoBoxOutput("dbInfoBoxNoRoomsMax"),
                                         infoBoxOutput("dbInfoBoxNoRoomsQ25"),
                                         infoBoxOutput("dbInfoBoxNoRoomsQ75")
                                     ),
                                     fluidRow(
                                         column(12,
                                            box(width = NULL, solidHeader = TRUE, status = "primary", title = tagList(tags$span("Verteilung der Daten im Boxplot:",style="font-size:22px")),
                                                plotOutput("dbNoRoomsBoxplot")
                                            )
                                        )
                                    )
                                )
                            )
                        ),
                        #### Rechte Box: Deutschlandkarte und Drop-Downs
                        column(4,
                           box(width = NULL, status = "primary", title = tagList(tags$span("Region auswählen",style="font-size:22px")), solidHeader = TRUE, style="font-size:18px",
                               fluidRow(
                                   column(6,
                                          selectInput("dbBundesland", "Bundesland", choices = NULL)       
                                   ),
                                   column(6,
                                          selectInput("dbLandkreis", "Landkreis", choices = NULL)
                                   )
                               ),
                               fluidRow(
                                   column(12,plotOutput("dbPlotMapGermany",height = 600)),
                               ),
                               fluidRow(
                                   column(12, textOutput("selectedDatasetCount")
                                 )
                               )
                           )
                        )
                    )
                ), # Ende: tabItem - Dashboard
                ## ---------------------------------------------------------------------------------------------------------------------------------
                ## Dashboard-Tab: baseData
                ## ---------------------------------------------------------------------------------------------------------------------------------
                tabItem(tabName = "baseData",
                    fluidRow(
                        column(12, 
                            box(width = NULL, solidHeader = TRUE, status = "primary", title = "Der zugrunde liegende Datensatz als Tabelle:",
                                dataTableOutput("data_table")    
                            )
                        )
                    )
                ), # Ende: tabItem - BaseData
                ## ---------------------------------------------------------------------------------------------------------------------------------
                ## Dashboard-Tab: rpe (Mietpreisschaetzung)
                ## ---------------------------------------------------------------------------------------------------------------------------------
                tabItem(tabName = "rpe",
                    fluidRow(
                        #### Linke Box
                        column(width=5,
                            box(width=NULL, solidHeader = TRUE, status = "primary", title = "(1) Welche Parameter sollen bei der Schätzung berücksichtigt werden?",
                                column(12,
                                    #### Box: Region
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
                                    #### Box: Wohnungsparameter
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
                                    #### Box: Ausstattung
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
                                    #### Action-Button Mietpreisschaetzung
                                    actionButton("getPrice", "Mietpreis schätzen", width = "100%", class="btn-info", style="background-color:#00a65a; color:white")
                                )
                            )
                        ),
                        #### Rechte Box
                        column(7,
                            box(width=NULL, solidHeader = TRUE, status = "primary", title = "(2) Ergebnisse der Mietpreisschätzung:",
                                #### Zeile: Ergebniss der Mietreisschaetzung
                                fluidRow(
                                    column(12,
                                           valueBoxOutput("vBoxPrice", width = NULL)       
                                    )
                                ),
                                #### Zeile: Angaben zum Modell
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
                                                #column(4,valueBoxOutput("pValueFromFtest", width = NULL))
                                            )
                                        )
                                    )  
                                ),
                                #### Zeile: Residuendiagramme
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
                ) # Ende: tabItem - rpe (Mietpreisschaetzung)
            ) # Ende: tabItems
        ) # Ende: dashboardBody
    ) # Ende: dashboardPage
)
