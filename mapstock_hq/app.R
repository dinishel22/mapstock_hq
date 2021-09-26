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

#OPERATIONS


# Define UI for my mapping application 
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
        #Basemaps
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
        #Layer control
        addLayersControl(
            baseGroups = c("OSM", "Toner", "Toner Lite", "CartoDB Dark","CartoDB Positron","Esri Imagery"),
            options = layersControlOptions(collapsed = TRUE))
             
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
