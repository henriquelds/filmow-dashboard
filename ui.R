#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Filmow dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Movies Visualizer", tabName = "movvisu", icon = icon("dashboard")),
        menuItem("Density (runtime)", tabName = "densrun", icon = icon("th"))
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "movvisu", 
                # Boxes need to be put in a row (or column)
                fluidRow(
                    column(6,box(DT::dataTableOutput("table",width="80%"),width = 12)),
                    column(6,box(
                        title = "Controls",
                        sliderInput("slidervisu", "Select year range:", min=data_maxmin$min, max=data_maxmin$max, value = c(data_maxmin$min, data_maxmin$max),step=10),
                        width = 12)
                    )
                )
            ),
            tabItem(tabName = "densrun",
                fluidRow(
                    column(8,plotOutput("plotdensrun", height = 250)),
                    column(4,box(
                        title = "Controls",
                        sliderInput("slideryear", "Select year range:", min=data_maxmin$min, max=data_maxmin$max, value = c(data_maxmin$min, data_maxmin$max),step=10),
                        selectizeInput("selectizedir", "Select director:", choices=directors$name, options = list(maxItems = 1)),
                        width = 12)
                    )
                ),
                fluidRow(
                    column(8,plotOutput("plotdir", height = 250)),  
                )
            
            )
        )
    )
)
