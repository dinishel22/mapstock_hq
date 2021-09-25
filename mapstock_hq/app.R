#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(leaflet)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Defining App theme
    theme = bs_theme(version = 5, bootswatch = "cerulean"),

    # Application title
    
    titlePanel(h1("Map Stock HQ", align ='center')),

    # Search bar for stcok symbols or company names
    fluidRow(
        column(width = 8,
               offset = 2, 
               searchInput(
                   inputId = "search", label = "",
                   placeholder = "Search a stock",
                   btnSearch = icon("search"),
                   btnReset = icon("remove"),
                   width = "100%"
               )
               )
    ),
      
    # Show a map with the HQ of the searched stock 
    fluidRow(column(12,
    
        leafletOutput("stockmap", width = "100%", height = "550px")
    ))
    
    # Adding a footer
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$stockmap <- renderLeaflet({
        leaflet() %>%
            addTiles()
             
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
