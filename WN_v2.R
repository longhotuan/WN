#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(usethis)
library(feather)

water_nexus <- read_feather("dgd_min.feather")

first_country <- which(colnames(water_nexus) == 'Afghanistan')
last_country <- which(colnames(water_nexus) == 'Palestine')
first_lat <- which(colnames(water_nexus) == 'lat_Afghanistan')
last_lat <- which(colnames(water_nexus) == 'lat_Palestine')
first_long <- which(colnames(water_nexus) == 'long_Afghanistan')
last_long <- which(colnames(water_nexus) == 'long_Palestine')
first_year <- which(colnames(water_nexus) == '2008')
last_year <- which(colnames(water_nexus) == '2019')
first_topsector <- which(colnames(water_nexus) == 'Agriculture, forestry, fishing')
last_topsector <- which(colnames(water_nexus) == 'Water and sanitation')
first_typology <- which(colnames(water_nexus) == 'Administrative costs not included elsewhere')
last_typology <- which(colnames(water_nexus) == 'Sector budget support')
first_sector <- which(colnames(water_nexus) == 'Agriculture and livestock - Agrarian reform')
last_sector <- which(colnames(water_nexus) == 'Water supply and sanitation - Waste management/disposal')

#### ui #####

ui <- dashboardPage(
        # Dashboard header ####
        dashboardHeader(title="Water Nexus dashboard"),
        # Dashboard sidebar #### 
        dashboardSidebar(
                sidebarMenu(id="tab",
                            menuItem("About", 
                                     tabName = "about",
                                     icon = icon("info")),
                            menuItem("Project info", 
                                     tabName = "info",
                                     icon = icon("list-ol")), 
                            # menuItem("Cooperation", 
                            #          tabName = "coop",
                            #          icon = icon("handshake")),
                            menuItem("Actors",
                                     tabName="actor",
                                     icon = icon("landmark")),
                            menuItem("Budgets", 
                                     tabName = "budget",
                                     icon = icon("euro-sign")),
                            selectInput(inputId = "nation", label = "Select a country", 
                                        choices = c(All = "All", "Partner countries", colnames(water_nexus)[first_country:last_country])),
                            selectInput(inputId = "year", label = "Select a year",
                                        choices = c(All = "All", colnames(water_nexus)[first_year:last_year])),
                            selectInput(inputId = "sector", label = "Select a sector", 
                                        choices = c(All = "All", colnames(water_nexus)[first_topsector:last_topsector])),
                            selectInput(inputId = "aid", label = "Select a type of aid", 
                                        choices = c(All = "All", colnames(water_nexus)[first_typology:last_typology]))
                )
        ),
        # Dashboard body #### 
        dashboardBody(
                tabItems(
                        # Info tab content ####
                        tabItem(tabName = "info",
                                fluidRow(
                                        valueBoxOutput("project"),
                                        valueBoxOutput("money"),
                                        valueBoxOutput("period")
                                ),
                                fluidRow(
                                        box(title = "Project info", width = 12, height = 700,
                                            DT::dataTableOutput("table"
                                                                ,  width = "100%", height = 700
                                            )
                                        )
                                ),
                                fluidRow(
                                        box(title = "Project map", width = 12, 
                                            leafletOutput("map", width = "100%", height = 400)
                                        )
                                )
                        ), # end tabItem of info tab
                        # # Cooperation tab content (abandoned) ####
                        # tabItem(tabName = "coop",
                        #         fluidRow(
                        #                 valueBoxOutput("project1"),
                        #                 valueBoxOutput("money1"),
                        #                 valueBoxOutput("period1")
                        #         ),
                        #         fluidRow(
                        #                 box(title = "Type of cooperation", width = 12,
                        #                     plotlyOutput("coop")
                        #                 )
                        #         )
                        # ), # end of cooperation tab content
                        # Actor tab content ####
                        tabItem(tabName = "actor",
                                fluidRow(
                                        valueBoxOutput("project1"),
                                        valueBoxOutput("money1"),
                                        valueBoxOutput("period1")
                                ),
                                fluidRow(
                                        box(title = "Funding Actors", width = 12,
                                            plotlyOutput("budgetholder")
                                        )
                                ),
                                fluidRow(
                                        box(title = "Implementing organisations/channels of delivery", width = 12,
                                            plotlyOutput("contractor")
                                        )
                                ),
                                fluidRow(
                                        box(title = "Funding evolution", width = 12,
                                            plotlyOutput("allocation")
                                        )
                                )
                        ), # end tabItem of actor tab
                        # Budget-holders tab content ####
                        tabItem(tabName = "budget",
                                fluidRow(
                                        valueBoxOutput("project2"),
                                        valueBoxOutput("money2"),
                                        valueBoxOutput("period2")
                                ),
                                fluidRow(
                                        box(title = "Type of aid", width = 12,
                                            plotlyOutput("aidtype")
                                        )
                                ),
                                fluidRow(
                                        box(title = "Budget category", width = 12,
                                            plotlyOutput("budget")
                                        )
                                )
                        ), # end tabItem of budget tab
                        # About tab content ####
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(width = 12, 
                                            h2("About the dashboard"),
                                            hr(),
                                            h3("Projects in the Water Sector by Belgian Actors"),
                                            br(),
                                            h4("The Water Projects Dashboard is an interactive platform centralizing and displaying information about the water-related projects led by Belgian actors. This platform is  dynamic and aim to incorporate upcoming projects. So far, the platform mostly inventories the projects funded by the Belgian Ministry of Foreign Affairs, Development Cooperation and Humanitarian Aid (DGD) from 1998 to present days. However, we are building a broader database that includes projects funded by other funding organisms.")
                                        )
                                ),
                                fluidRow(
                                        box(width = 12, 
                                            h2("About the dataset"),
                                            hr(),
                                            h3("Dataset of Directorate-General for Development Cooperation and Humanitarian Aid"),
                                            br(),
                                            h4("The dataset of Directorate-General for Development Cooperation and Humanitarian Aid (DGD) documents the 12.550 projects involved in the Belgian ODA flows from 1987 to 2018. The dataset focuses on projects working in the water sector which can involve in multidisciplinary themes. Hence, projects with themes related environment, agriculture, fisheries, forestry, and hydroelectricity are included in the dataset. The dataset contains in total 191 attributes which are characteristics of any projects that have cooperation with DGD. The attributes cover from basic information of the projects, e.g. title, year, period, etc., to specific properties of the projects, i.e. scale of their involvement with respect to Sustainable Development Goals (SDGs), target groups, reached results, etc. However, due to substantial missing values, only main attributes are exploited in this dashboard."),
                                            br(),
                                            h4("Besides this dataset, a broader database that includes projects funded by other funding organizations, such as VLIR-UOS, ARES, VPWvO, Enabel, etc., is being developed. If you want to add the information about the projects funded/implemented by your organisation, please send us an email to: ",
                                               a("waternexusbelgium@gmail.com",
                                                 href = "mailto: waternexusbelgium@gmail.com"))
                                        )
                                ),
                                fluidRow(
                                        column(6,
                                               h1("Funded by"),
                                               img(style = "max-width:50%",
                                                   src = "Logo2.jpg")
                                        ),
                                        column(6, 
                                               img(align = "left|bottom",
                                                   style = "max-width:50%",
                                                   src = "Logo.jpg") 
                                        )
                                ),
                                fluidRow(
                                        column(6,
                                               h2("Through"),
                                               box(
                                                       img(style = "max-width:100%",
                                                           src = "Logo3.jpg")
                                               ),
                                               box(
                                                       img(style = "max-width:100%",
                                                           src = "Logo4.png")
                                               )
                                        )
                                )
                        ) # end about tabItem
                ) # end tabItems
        ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output, session) {
        # Setting reactivities ####
        df <- reactive({water_nexus})
        
        #** Nation ####
        
        nationname <- reactive({
                nationname <- vector(mode = 'character', length = 0)
                for (i in first_country:last_country){
                        if(sum(!is.na(df()[,i] > 0))){
                                nationname <- c(nationname, colnames(df()[,i]))
                        }
                }
                nationname
        })
        
        observe({
                updateSelectInput(session, inputId = "nation", label = "Select a country", choices = c("All", "Partner countries", sort(nationname())))
        })
        
        df_country <- reactive({
                input$nation
        })
        
        #** Year ####

        year_name <- reactive({
                if(df_country() == "All"){
                        colnames(df())[first_year:last_year]
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco",
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                        year_name2 <- df()[rowSums(is.na(m)) != ncol(m), ]
                        year_name <- vector(mode = "character", length = 0)
                        for (i in first_year:last_year){
                                if(sum(!is.na(year_name2[,i]))>0){
                                        year_name <- c(year_name, colnames(year_name2[,i]))
                                }
                        }
                        year_name
                } else {
                        year_name2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        year_name <- vector(mode = "character", length = 0)
                        for (i in first_year:last_year){
                                if(sum(!is.na(year_name2[,i]))>0){
                                        year_name <- c(year_name, colnames(year_name2[,i]))
                                }
                        }
                        year_name
                }
        })

        observe({
                updateSelectInput(session, inputId = "year", label = "Select a year", choices = c("All", sort(year_name())))
        })

        df_year <- reactive({
                input$year
        })
        
        #** Top Sector ####
        
        sector_name <- reactive({
                if(df_country() == "All"){
                        sectorname <- df()
                        if(df_year() == "All"){
                                sector_name <- sectorname$`TOP SECTOR`
                        } else {
                                sector_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_year()]),]
                                sector_name <- sector_name2$`TOP SECTOR`
                                sector_name
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        sectorname <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        sectorname <- sectorname[complete.cases(sectorname[ ,1]),] # remove the one that has NA in all row
                        if(df_year() == "All"){
                                sector_name <- sectorname$`TOP SECTOR`
                        } else {
                                sector_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_year()]),]
                                sector_name <- sector_name2$`TOP SECTOR`
                        }
                        
                } else {
                        sectorname <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                sector_name <- sectorname$`TOP SECTOR`
                        } else {
                                sector_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_year()]),]
                                sector_name <- sector_name2$`TOP SECTOR`
                                sector_name
                        }
                }
        })
        
        observe({
                updateSelectInput(session, inputId = "sector", label = "Select a sector", choices = c("All", sort(sector_name())))
        })
        
        df_sector <- reactive({
                input$sector
        })
        
        #** Type of aid #### 
        aid_name <- reactive({
                if(df_country() == "All"){
                        yearname <- df()
                        if(df_year() == "All"){
                                
                                if(df_sector() == "All"){
                                        aid_name <- yearname$TYPOLOGY       
                                } else {
                                        aid_name2 <- yearname[!is.na(yearname[, colnames(yearname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        } else {
                                sectorname <- yearname[!is.na(yearname[, colnames(yearname) == df_year()]),]
                                # sectorname <- sectorname2$`TOP SECTOR`
                                # sectorname
                                if(df_sector() == "All"){
                                        aid_name <- sectorname$TYPOLOGY
                                } else {
                                        aid_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        }
                                
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        yearname <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        yearname <- yearname[complete.cases(yearname[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        aid_name <- yearname$TYPOLOGY       
                                } else {
                                        aid_name2 <- yearname[!is.na(yearname[, colnames(yearname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        } else {
                                sectorname <- yearname[!is.na(yearname[, colnames(yearname) == df_year()]),]
                                # sectorname <- sectorname2$`TOP SECTOR`
                                # sectorname
                                if(df_sector() == "All"){
                                        aid_name <- sectorname$TYPOLOGY
                                } else {
                                        aid_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        }
                } else {
                        yearname <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        aid_name <- yearname$TYPOLOGY       
                                } else {
                                        aid_name2 <- yearname[!is.na(yearname[, colnames(yearname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        } else {
                                sectorname <- yearname[!is.na(yearname[, colnames(yearname) == df_year()]),]
                                # sectorname <- sectorname2$`TOP SECTOR`
                                # sectorname
                                if(df_sector() == "All"){
                                        aid_name <- sectorname$TYPOLOGY
                                } else {
                                        aid_name2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_sector()]),]
                                        aid_name <- aid_name2$TYPOLOGY
                                        aid_name
                                }
                        }
                }
        })
        
        observe({
                updateSelectInput(session, inputId = "aid", label = "Select a type of aid", choices = c("All", sort(aid_name())))
        })
        df_aid <- reactive({
                input$aid
        })
        
        # Output valuebox in info tab ####
        output$project <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = nrow(selected_df),
                        subtitle = "Total number of project",
                        icon = icon("list-ol"),
                        color = "purple"
                )
        })
        output$money <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = prettyNum(sum(selected_df$TOTAL_BUDGET, na.rm = TRUE), big.mark = ","),
                        subtitle = "Total budget (in EUR)",
                        icon = icon("euro-sign"),
                        color = "yellow"
                )
        })
        output$period <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = max(selected_df$`last-year-exp`, na.rm = TRUE) - min(selected_df$`first-year-exp`, na.rm = TRUE),
                        subtitle = "Total period (years)",
                        icon = icon("calendar"),
                        color = "blue"
                )
        })
        # Output valuebox in actors tab ####
        output$project1 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = nrow(selected_df),
                        subtitle = "Total number of project",
                        icon = icon("list-ol"),
                        color = "purple"
                )
        })
        output$money1 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = prettyNum(sum(selected_df$TOTAL_BUDGET, na.rm = TRUE), big.mark = ","),
                        subtitle = "Total budget (in EUR)",
                        icon = icon("euro-sign"),
                        color = "yellow"
                )
        })
        output$period1 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = max(selected_df$`last-year-exp`, na.rm = TRUE) - min(selected_df$`first-year-exp`, na.rm = TRUE),
                        subtitle = "Total period (years)",
                        icon = icon("calendar"),
                        color = "blue"
                )
        })
        # Output valuebox in budgets tab ####
        output$project2 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = nrow(selected_df),
                        subtitle = "Total number of project",
                        icon = icon("list-ol"),
                        color = "purple"
                )
        })
        output$money2 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = prettyNum(sum(selected_df$TOTAL_BUDGET, na.rm = TRUE), big.mark = ","),
                        subtitle = "Total budget (in EUR)",
                        icon = icon("euro-sign"),
                        color = "yellow"
                )
        })
        output$period2 <- renderValueBox({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                valueBox(
                        value = max(selected_df$`last-year-exp`, na.rm = TRUE) - min(selected_df$`first-year-exp`, na.rm = TRUE),
                        subtitle = "Total period (years)",
                        icon = icon("calendar"),
                        color = "blue"
                )
        })
        # # Output valuebox3 (abandoned) ####
        # output$project3 <- renderValueBox({
        #         selectedData <- df()
        #         if (df_country() == "All"){
        #                 if (df_year() == "All") {
        #                         selectedData <- df()
        #                 } else {
        #                         selectedData <- df() %>% filter(between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         } else {
        #                 if (df_year() == "All") {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country())
        #                 } else {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country() & between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         }
        #         valueBox(
        #                 value = nrow(selectedData),
        #                 subtitle = "Total number of project",
        #                 icon = icon("list-ol"), 
        #                 color = "purple"
        #         )
        # })
        # output$money3 <- renderValueBox({
        #         selectedData <- df()
        #         if (df_country() == "All"){
        #                 if (df_year() == "All") {
        #                         selectedData <- df()
        #                 } else {
        #                         selectedData <- df() %>% filter(between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         } else {
        #                 if (df_year() == "All") {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country())
        #                 } else {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country() & between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         }
        #         valueBox(
        #                 value =  prettyNum(sum(selectedData$TOTAL_BUDGET, na.rm = TRUE), big.mark = ","),
        #                 subtitle = "Total budget (in EUR)", 
        #                 icon = icon("euro-sign"), 
        #                 color = "yellow"
        #         )
        # })
        # output$period3 <- renderValueBox({
        #         selectedData <- df()
        #         if (df_country() == "All"){
        #                 if (df_year() == "All") {
        #                         selectedData <- df()
        #                 } else {
        #                         selectedData <- df() %>% filter(between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         } else {
        #                 if (df_year() == "All") {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country())
        #                 } else {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country() & between(df_year(), X1st.year.exp, last.year.exp))
        #                 }
        #         }
        #         valueBox(
        #                 value = max(selectedData$last.year.exp, na.rm = TRUE) - min(selectedData$X1st.year.exp, na.rm = TRUE), 
        #                 subtitle = "Total period (years)", 
        #                 icon = icon("calendar"), 
        #                 color = "blue"
        #         )
        # })
        # Output table in info tab ####
        output$table <- DT::renderDataTable(
                server = FALSE,
                {
                        if(df_country() == "All"){
                                selected_df3 <- df()
                                if(df_year() == "All"){
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df3
                                                } else {
                                                        selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                } else {
                                        selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df1
                                                } else {
                                                        selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                }
                        } else if(df_country() == "Partner countries"){
                                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                                selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                                selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                                
                                if(df_year() == "All"){
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df3
                                                } else {
                                                        selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                } else {
                                        selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df1
                                                } else {
                                                        selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                }
                        } else {
                                selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                                if(df_year() == "All"){
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df3
                                                } else {
                                                        selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                } else {
                                        selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                        if(df_sector() == "All"){
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df1
                                                } else {
                                                        selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                                }
                                        } else {
                                                selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                                if(df_aid() == "All"){
                                                        selected_df <- selected_df2
                                                } else {
                                                        selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                                }
                                        }
                                }
                        }
                        description_df <- selected_df[, which(colnames(selected_df) == "BACKGROUND (IN)"):which(colnames(selected_df) == "RESULTS_OUTPUT (CO)")] %>% 
                                tidyr::unite(Description, remove = TRUE, sep = ". ", na.rm = TRUE)
                        selected_df <- selected_df %>% select(TITLE_GLOBAL, COUNTRY, TYPOLOGY, CONTRACTOR, TOTAL_BUDGET, `TOP SECTOR`, `first-year-exp`,`last-year-exp`)
                        selected_df <- bind_cols(selected_df, description_df)
                        colnames(selected_df) <- c("Title", "Funded countries", "Type of aids", "Contractors", "Budget (in EUR)", 
                                                   "Sectors", "First year", "Last year", "Description")
                        
                        # selectedData$`Budget (in EUR)` <- as.numeric(selectedData$`Budget (in EUR)`)
                        DT::datatable(selected_df,
                                      rownames = FALSE,
                                      filter="top",
                                      selection="multiple",
                                      escape=FALSE,
                                      extensions = 'Buttons',
                                      options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                                                     pageLength = 5,
                                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     dom = 't',
                                                     scrollX = TRUE,
                                                     fixedColumns = FALSE)) %>% formatCurrency(5, currency = "", digits = 2)


                })
        # Output map in info tab####
        output$map <- renderLeaflet({
                tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
                colors <- brewer.pal(n = 7, name = "Dark2")
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                WN_leaflet_2 <- selected_df %>%
                        dplyr::group_by(`TOP SECTOR`, COUNTRY, lat, long) %>%
                        dplyr::summarise(Count=n()) %>%
                        dplyr::ungroup()
                WN_leaflet_3 <- spread(WN_leaflet_2, key = "TOP SECTOR", value = "Count")
                WN_leaflet_3$Total <- rowSums(subset(WN_leaflet_3, select = -c(COUNTRY, lat, long)), na.rm = TRUE)
                leaflet() %>%
                        addTiles(tilesURL) %>%
                        fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
                        addMinicharts(WN_leaflet_3$lat, WN_leaflet_3$long,
                                      type = "pie",
                                      chartdata = subset(WN_leaflet_3, select = -c(COUNTRY, lat, long, Total)),
                                      colorPalette = colors,
                                      width = 80 * sqrt(WN_leaflet_3$Total) / sqrt(max(WN_leaflet_3$Total)),
                                      transitionTime = 0)
        })

        # # Function for choosing different country and time (abandoned) ####
        # all_country <- function(x){
        #         x <- enquo(x)
        #         if (df_country() == "All"){
        #                 if (df_year() == "All") {
        #                         selectedData <- df() %>% group_by(!!x) %>% summarise(Count = n()) %>%
        #                                 arrange(desc(Count)) %>% ungroup()
        #                 } else {
        #                         selectedData <- df() %>% filter(between(df_year(), X1st.year.exp, last.year.exp)) %>% group_by(!!x) %>%
        #                                 summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
        #                 }
        #         } else {
        #                 if (df_year() == "All") {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country()) %>% group_by(!!x) %>%
        #                                 summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
        #                 } else {
        #                         selectedData <- df() %>% filter(COUNTRY == df_country() & between(df_year(), X1st.year.exp, last.year.exp)) %>%
        #                                 group_by(!!x) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
        #                 }
        #         }
        #         return(selectedData)
        # }
        # # Output cooperation in cooperation tab (abandoned) ####
        # output$coop <-renderPlotly({
        #         selectedData <- all_country(COOPERATION)
        #         coop <-plot_ly(selectedData,
        #                        labels = ~ COOPERATION,
        #                        values = ~ Count, 
        #                        type ="pie",
        #                        insidetextfont = list(color = "#FFFFFF"), 
        #                        textfont = list(color = '#000000', size = 12)
        #         ) %>%
        #                 layout(
        #                         legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
        #                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        #                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        # })
        # 
        # Output budgetholder~funding actors in actors tab ####
        output$budgetholder <-renderPlotly({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                df <- selected_df %>% dplyr::group_by(BUDGETHOLDER) %>%
                        dplyr::summarise(Count = n()) %>% 
                        dplyr::arrange(desc(Count))
                selected_df$TOTAL_BUDGET[is.na(selected_df$TOTAL_BUDGET)] <- 0
                df2 <- aggregate(data = selected_df, TOTAL_BUDGET ~ BUDGETHOLDER, FUN = sum) %>%
                        dplyr::mutate(Percentage = TOTAL_BUDGET*100/sum(TOTAL_BUDGET)) %>%
                        dplyr::arrange(desc(TOTAL_BUDGET))
                df <- dplyr::inner_join(df, df2)
                p <- plot_ly(data = df,
                             labels = ~ BUDGETHOLDER,
                             values = ~ Count,
                             type ="pie",
                             insidetextfont = list(color = "#FFFFFF"),
                             name = "% project",
                             textfont = list(color = '#000000', size = 12),
                             domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = df,
                                labels = ~ BUDGETHOLDER,
                                values = ~ TOTAL_BUDGET,
                                type ="pie",
                                insidetextfont = list(color = "#FFFFFF"),
                                name = "% budget",
                                textfont = list(color = '#000000', size = 12),
                                domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                grid=list(rows=1, columns=2),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                p

        })

        # Output contractor~Implementing organisations/channels of delivery in actors tab ####
        output$contractor <-renderPlotly({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                df <- selected_df %>% dplyr::group_by(TOP_CONTRACTOR) %>%
                        dplyr::summarise(Count = n()) %>% 
                        dplyr::arrange(desc(Count))
                selected_df$TOTAL_BUDGET[is.na(selected_df$TOTAL_BUDGET)] <- 0
                df2 <- aggregate(data = selected_df, TOTAL_BUDGET ~ TOP_CONTRACTOR, FUN = sum) %>%
                        dplyr::mutate(Percentage = TOTAL_BUDGET*100/sum(TOTAL_BUDGET)) %>%
                        dplyr::arrange(desc(TOTAL_BUDGET))
                df <- dplyr::inner_join(df, df2)
                p <- plot_ly(data = df,
                             labels = ~ TOP_CONTRACTOR,
                             values = ~ Count,
                             type ="pie",
                             insidetextfont = list(color = "#FFFFFF"),
                             name = "% project",
                             textfont = list(color = '#000000', size = 12),
                             domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = df,
                                labels = ~ TOP_CONTRACTOR,
                                values = ~ TOTAL_BUDGET,
                                type ="pie",
                                insidetextfont = list(color = "#FFFFFF"),
                                name = "% budget",
                                textfont = list(color = '#000000', size = 12),
                                domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                               grid=list(rows=1, columns=2),
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                p
        })


        # Output allocation~Funding evolution in actors tab ####
        output$allocation <-renderPlotly({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                
                allocation <- aggregate(TOTAL_BUDGET~`first-year-exp`+BUDGETHOLDER, data = selected_df, sum)
                colnames(allocation) <- c('Year', "Funding actors", "Budget")
                allocation$Year <- as.factor(allocation$Year)
                ggplotly(ggplot(allocation, aes(x = Year,  y = Budget, color = `Funding actors`, group = `Funding actors`)) +
                                 geom_point(size = 2)+
                                 geom_line(size = 1.1125)+
                                 theme_bw() +
                                 xlab("Year") +
                                 ylab("Budget allocation (in EUR)") +
                                 theme(text=element_text(family = "Arial")) +
                                 theme(axis.text.x = element_text(size = 9)) +
                                 theme(axis.text.y = element_text(size = 9)) +
                                 theme(axis.title = element_text(size = 10)) +
                                 theme(legend.title = element_text(size = 11)) +
                                 theme(legend.text = element_text(size = 10))
                         )
        })
        # Output aid~Type of aid in budgets tab ####
        output$aidtype <- renderPlotly({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                df <- selected_df %>% dplyr::group_by(TYPOLOGY) %>%
                        dplyr::summarise(Count = n()) %>% 
                        dplyr::arrange(desc(Count))
                selected_df$TOTAL_BUDGET[is.na(selected_df$TOTAL_BUDGET)] <- 0
                df2 <- aggregate(data = selected_df, TOTAL_BUDGET ~ TYPOLOGY, FUN = sum) %>%
                        dplyr::mutate(Percentage = TOTAL_BUDGET*100/sum(TOTAL_BUDGET)) %>%
                        dplyr::arrange(desc(TOTAL_BUDGET))
                df <- dplyr::inner_join(df, df2)
                p <- plot_ly(data = df,
                             labels = ~ TYPOLOGY,
                             values = ~ Count,
                             type ="pie",
                             insidetextfont = list(color = "#FFFFFF"),
                             name = "% project",
                             textfont = list(color = '#000000', size = 12),
                             domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = df,
                                labels = ~ TYPOLOGY,
                                values = ~ TOTAL_BUDGET,
                                type ="pie",
                                insidetextfont = list(color = "#FFFFFF"),
                                name = "% budget",
                                textfont = list(color = '#000000', size = 12),
                                domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                               grid=list(rows=1, columns=2),
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                p
        })

        # Output budget~ Budget category in budgets tab ####
        output$budget <- renderPlotly({
                if(df_country() == "All"){
                        selected_df3 <- df()
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else if(df_country() == "Partner countries"){
                        m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
                        selected_df3 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
                        selected_df3 <- selected_df3[complete.cases(selected_df3[ ,1]),] # remove the one that has NA in all row
                        
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                } else {
                        selected_df3 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                        if(df_year() == "All"){
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df3
                                        } else {
                                                selected_df <- selected_df3[selected_df3$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        } else {
                                selected_df1 <- selected_df3[!is.na(selected_df3[, colnames(selected_df3) == df_year()]),]
                                if(df_sector() == "All"){
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df1
                                        } else {
                                                selected_df <- selected_df1[selected_df1$TYPOLOGY == df_aid(),]
                                        }
                                } else {
                                        selected_df2 <- selected_df1[!is.na(selected_df1[, colnames(selected_df1) == df_sector()]),]
                                        if(df_aid() == "All"){
                                                selected_df <- selected_df2
                                        } else {
                                                selected_df <- selected_df2[selected_df2$TYPOLOGY == df_aid(),]
                                        }
                                }
                        }
                }
                df <- selected_df %>% dplyr::group_by(BUDGET) %>%
                        dplyr::summarise(Count = n()) %>% 
                        dplyr::arrange(desc(Count))
                selected_df$TOTAL_BUDGET[is.na(selected_df$TOTAL_BUDGET)] <- 0
                df2 <- aggregate(data = selected_df, TOTAL_BUDGET ~ BUDGET, FUN = sum) %>%
                        dplyr::mutate(Percentage = TOTAL_BUDGET*100/sum(TOTAL_BUDGET)) %>%
                        dplyr::arrange(desc(TOTAL_BUDGET))
                df <- dplyr::inner_join(df, df2)
                p <- plot_ly(data = df,
                             labels = ~ BUDGET,
                             values = ~ Count,
                             type ="pie",
                             insidetextfont = list(color = "#FFFFFF"),
                             name = "% project",
                             textfont = list(color = '#000000', size = 12),
                             domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = df,
                                labels = ~ BUDGET,
                                values = ~ TOTAL_BUDGET,
                                type ="pie",
                                insidetextfont = list(color = "#FFFFFF"),
                                name = "% budget",
                                textfont = list(color = '#000000', size = 12),
                                domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                               grid=list(rows=1, columns=2),
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                p
        })
}

#### Run the application ####
shinyApp(ui = ui, server = server)
