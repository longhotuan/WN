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

Water_Nexus <- read.csv('Water_Nexus.csv',encoding = "UTF8")
levels(Water_Nexus$TYPOLOGY)[2] <- "Contributions to specific-purpose programs"
levels(Water_Nexus$TYPOLOGY)[3] <- "Core support to NGOs and other organizations"

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
                            menuItem("Cooperation", 
                                     tabName = "coop",
                                     icon = icon("handshake")),
                            menuItem("Funding actors",
                                     tabName="actor",
                                     icon = icon("landmark")),
                            menuItem("Budgets", 
                                     tabName = "budget",
                                     icon = icon("euro-sign")),
                            selectInput(inputId = "country", label = "Select a country", 
                                        choices = c(All = "All",levels(Water_Nexus$COUNTRY))),
                            selectInput(inputId = "year", label = "Select the first year",
                                        choices = c(All = "All",levels(as.factor(Water_Nexus$X1st.year.exp))))
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
                        # Cooperation tab content ####
                        tabItem(tabName = "coop",
                                fluidRow(
                                        valueBoxOutput("project1"),
                                        valueBoxOutput("money1"),
                                        valueBoxOutput("period1")
                                ),
                                fluidRow(
                                        box(title = "Type of cooperation", width = 12,
                                            plotlyOutput("coop")
                                        )
                                ),
                                fluidRow(
                                        box(title = "Contractors", width = 12,
                                            plotlyOutput("contractor")
                                        )
                                )
                        ), # end of cooperation tab content
                        # Actor tab content ####
                        tabItem(tabName = "actor",
                                fluidRow(
                                        valueBoxOutput("project2"),
                                        valueBoxOutput("money2"),
                                        valueBoxOutput("period2")
                                ),
                                fluidRow(
                                        box(title = "Funding Actors", width = 12,
                                            plotlyOutput("budgetholder")
                                        )
                                ),
                                fluidRow(
                                        box(title = "Funding revolution", width = 12,
                                            plotlyOutput("allocation")
                                        )
                                )
                        ), # end tabItem of actor tab
                        # Budget-holders tab content ####
                        tabItem(tabName = "budget",
                                fluidRow(
                                        valueBoxOutput("project3"),
                                        valueBoxOutput("money3"),
                                        valueBoxOutput("period3")
                                ),
                                fluidRow(
                                        box(title = "Aid category", width = 12,
                                            plotlyOutput("aid")
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
                                            h4("The dataset of Directorate-General for Development Cooperation and Humanitarian Aid (DGD) contains 12550 projects in total from 1987 to 2018. The dataset mainly focuses on projects in the water sector. As such, projects related to including environment, agriculture, fisheries, forestry, and hydroelectricity are also included. The dataset includes 191 attributes which are characteristics of any projects that have cooperation with DGD. The attributes cover from basic information of the projects, e.g. title, year, period, etc., to specific properties of the projects, i.e. scale of their involvement with respect to Sustainable Development Goals (SDGs), target groups, reached results, etc."),
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
                                )
                        ) # end about tabItem
                ) # end tabItems
        ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output, session) {
        # Setting reactivities ####
        df <- reactive({Water_Nexus})
        df_country <- reactive({
                input$country
        })
        observe({
                updateSelectInput(session, inputId = "year",label = "Select the first year",
                                  choices = c(All = "All", levels(as.factor(df()$X1st.year.exp[df()$COUNTRY == df_country()]))))
        })
        df_year <- reactive({
                input$year
        })
        
        # Output valuebox ####
        output$project <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = nrow(selectedData),
                        subtitle = "Total project",
                        icon = icon("list-ol"), 
                        color = "purple"
                )
        })
        output$money <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = sum(selectedData$TOTAL_BUDGET, na.rm = TRUE),
                        subtitle = "Total budget (euros)", 
                        icon = icon("euro-sign"), 
                        color = "yellow"
                )
        })
        output$period <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = max(selectedData$last.year.exp, na.rm = TRUE) - min(selectedData$X1st.year.exp, na.rm = TRUE), 
                        subtitle = "Total period (years)", 
                        icon = icon("calendar"), 
                        color = "blue"
                )
        })
        # Output table  ####
        output$table <- DT::renderDataTable(
                server = FALSE,
                                            {
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                selectedData <- selectedData %>% select(TITLE_ENG, COUNTRY, COOPERATION, CONTRACTOR, TOTAL_BUDGET, TOP.SECTOR, X1st.year.exp,last.year.exp)
                colnames(selectedData) <- c("Title", "Funded countries", "Cooperation", "Contractors", "Budget", "Sector", "First year", "Last year")
                DT::datatable(selectedData, 
                              filter="top", 
                              selection="multiple", 
                              escape=FALSE, 
                              extensions = 'Buttons',
                              options = list(sDom  = '<"top"pB>t<"bottom"i>r', pageLength = 5, 
                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
                              
        })
        # Output map ####
        output$map <- renderLeaflet({
                tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
                colors <- brewer.pal(n = 7, name = "Dark2")
                WN_leaflet <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                WN_leaflet <- df()
                        } else {
                                WN_leaflet <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                WN_leaflet <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                WN_leaflet <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                WN_leaflet_2 <- WN_leaflet %>%
                        group_by(TOP.SECTOR, COUNTRY, lat, long) %>%
                        summarise(n=n()) %>%
                        ungroup()
                WN_leaflet_3 <- spread(WN_leaflet_2, key = "TOP.SECTOR", value = "n")
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
        
        # Function for choosing different country and time ####
        all_country <- function(x){
                x <- enquo(x)
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df() %>% group_by(!!x) %>% summarise(Count = n()) %>%
                                        arrange(desc(Count)) %>% ungroup()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year()) %>% group_by(!!x) %>%
                                        summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country()) %>% group_by(!!x) %>%
                                        summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year()) %>%
                                        group_by(!!x) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup()
                        }
                }
                return(selectedData)
        }
        # Output valuebox1 ####
        output$project1 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = nrow(selectedData),
                        subtitle = "Total project",
                        icon = icon("list-ol"), 
                        color = "purple"
                )
        })
        output$money1 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = sum(selectedData$TOTAL_BUDGET, na.rm = TRUE),
                        subtitle = "Total budget (euros)", 
                        icon = icon("euro-sign"), 
                        color = "yellow"
                )
        })
        output$period1 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = max(selectedData$last.year.exp, na.rm = TRUE) - min(selectedData$X1st.year.exp, na.rm = TRUE), 
                        subtitle = "Total period (years)", 
                        icon = icon("calendar"), 
                        color = "blue"
                )
        })
        # Output valuebox2 ####
        output$project2 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = nrow(selectedData),
                        subtitle = "Total project",
                        icon = icon("list-ol"), 
                        color = "purple"
                )
        })
        output$money2 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = sum(selectedData$TOTAL_BUDGET, na.rm = TRUE),
                        subtitle = "Total budget (euros)", 
                        icon = icon("euro-sign"), 
                        color = "yellow"
                )
        })
        output$period2 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = max(selectedData$last.year.exp, na.rm = TRUE) - min(selectedData$X1st.year.exp, na.rm = TRUE), 
                        subtitle = "Total period (years)", 
                        icon = icon("calendar"), 
                        color = "blue"
                )
        })
        # Output valuebox3 ####
        output$project3 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = nrow(selectedData),
                        subtitle = "Total project",
                        icon = icon("list-ol"), 
                        color = "purple"
                )
        })
        output$money3 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = sum(selectedData$TOTAL_BUDGET, na.rm = TRUE),
                        subtitle = "Total budget (euros)", 
                        icon = icon("euro-sign"), 
                        color = "yellow"
                )
        })
        output$period3 <- renderValueBox({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                valueBox(
                        value = max(selectedData$last.year.exp, na.rm = TRUE) - min(selectedData$X1st.year.exp, na.rm = TRUE), 
                        subtitle = "Total period (years)", 
                        icon = icon("calendar"), 
                        color = "blue"
                )
        })
        # Output cooperation ####
        output$coop <-renderPlotly({
                selectedData <- all_country(COOPERATION)
                coop <-plot_ly(selectedData,
                               labels = ~ COOPERATION,
                               values = ~ Count, 
                               type ="pie",
                               insidetextfont = list(color = "#FFFFFF"), 
                               textfont = list(color = '#000000', size = 12)
                ) %>%
                        layout(
                                legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        
        # Output contractor ####
        output$contractor <-renderPlotly({
                selectedData <- all_country(TOP_CONTRACTOR)
                contractor <-plot_ly(selectedData,
                                     labels = ~ TOP_CONTRACTOR,
                                     values = ~ Count,
                                     type ="pie",
                                     insidetextfont = list(color = "#FFFFFF"), 
                                     textfont = list(color = '#000000', size = 12)
                ) %>%
                        layout(
                                legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        
        # Output funding ####
        output$budgetholder <-renderPlotly({
                selectedData <- all_country(BUDGETHOLDER)
                budget <-plot_ly(selectedData,
                                 labels = ~ BUDGETHOLDER,
                                 values = ~ Count,
                                 type ="pie",
                                 insidetextfont = list(color = "#FFFFFF"), 
                                 textfont = list(color = '#000000', size = 12)
                ) %>%
                        layout(
                                legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
        })
        
        # Output aid ####
        output$aid <- renderPlotly({
                selectedData <- all_country(TYPOLOGY)
                aid <-plot_ly(selectedData,
                              labels = ~ TYPOLOGY,
                              values = ~ Count,
                              type ="pie",
                              insidetextfont = list(color = "#FFFFFF"), 
                              textfont = list(color = '#000000', size = 12)
                ) %>%
                        layout(
                                legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        
        # Output budget ####
        output$budget <- renderPlotly({
                selectedData <- all_country(BUDGET)
                budget <-plot_ly(selectedData,
                                 labels = ~ BUDGET,
                                 values = ~ Count,
                                 type ="pie",
                                 insidetextfont = list(color = "#FFFFFF"), 
                                 textfont = list(color = '#000000', size = 12)
                ) %>%
                        layout(
                                legend = list(orientation = 'h', font = list(size = 11), xanchor = "center", x = 0.5),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        # Output funding revolution ####
        output$allocation <-renderPlotly({
                selectedData <- df()
                if (df_country() == "All"){
                        if (df_year() == "All") {
                                selectedData <- df()
                        } else {
                                selectedData <- df() %>% filter(X1st.year.exp == df_year())
                        }
                } else {
                        if (df_year() == "All") {
                                selectedData <- df() %>% filter(COUNTRY == df_country())
                        } else {
                                selectedData <- df() %>% filter(COUNTRY == df_country() & X1st.year.exp == df_year())
                        }
                }
                allocation <- aggregate(TOTAL_BUDGET~X1st.year.exp+BUDGETHOLDER, data = selectedData, sum)
                colnames(allocation) <- c('Year', "Funding actors", "Budget")
                allocation$Year <- as.factor(allocation$Year)
                ggplotly(ggplot(allocation, aes(x = Year,  y = Budget, color = `Funding actors`, group = `Funding actors`)) +
                                 geom_point(size = 2)+
                                 geom_line(size = 1.1125)+
                                 theme_bw() +
                                 xlab("Year") +
                                 ylab("Budget allocation (euros)") +
                                 theme(text=element_text(family = "Arial")) +
                                 theme(axis.text.x = element_text(size = 9)) +
                                 theme(axis.text.y = element_text(size = 9)) +
                                 theme(axis.title = element_text(size = 10)) +
                                 theme(legend.title = element_text(size = 11)) +
                                 theme(legend.text = element_text(size = 10))
                         )
        })
}

#### Run the application ####
shinyApp(ui = ui, server = server)
