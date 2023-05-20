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
#Scrape the data from a website
library(rvest)
#To be used by the stringr package
library(tidyverse)
#To manipulate/clean the address string from rvest   
library(stringr)
#
library(dplyr)
# Put the data in structure tydygeocoder understands, tibble
library(tibble)
#To turn address into geographical coordenates
library(tidygeocoder)
#Transform the data tibble into a mapping-ready spatial object/dataframe
library(sf)


### Getting the address data from Yahoo Fincance website 
scrape_address <- function(stock_symbol){
  search_url<- paste0("https://finance.yahoo.com/quote/", 
                      stock_symbol, 
                      "/profile?p=", 
                      stock_symbol)
  scrape_result <- read_html(search_url) %>% 
    html_nodes(".asset-profile-container p:first-child") %>% 
    html_text2()  
  return (scrape_result)
}

### Cleaning the address so it is ready to be used by the Geocoder
address_cleaner <- function(stock_search){
  address_to_clean <- scrape_address(stock_search)
  
  unwanted_pttrns <- str_c(c("react-text: \\d+","/react-text"),
                           collapse = "|")
  
  clean_address <- str_remove_all(address_to_clean,unwanted_pttrns)
  clean_address <- str_split(clean_address,"\n")[[1]][1:4]
  clean_address<- str_c(clean_address,collapse = "")
  
  return(clean_address)
}


### Not strictly necessary, but decided to company name separately for now
scrape_name <- function(stock_symbol){
  search_url<- paste0("https://finance.yahoo.com/quote/", 
                      stock_symbol, 
                      "/profile?p=", 
                      stock_symbol)
  scrape_result <- read_html(search_url) %>% 
    html_nodes(".asset-profile-container h3") %>% 
    html_text2()  
  return (scrape_result)
}

### Use the clean address in the Geocoder to get the coordinates
geocode_stock <- function(stock_search){
  #get the name of the company
  company_name <- scrape_name(stock_search)
  #get the address of company's HQ
  company_address <- address_cleaner(stock_search)
  #Join name and address into a data table/tibble
  address_tibble <- tribble(
    ~name,                  ~addr,
    company_name,          company_address)
  #geocodes the address, adds lat&long coordinates to the table/tibble
  lat_longs <- address_tibble %>%
    geocode( addr,method = "arcgis", lat = lat , long = lon)
  # we,re good to go, but we'll add an extra step
  # We'll create an sf point layer, for easier mapping
  stock_pt_layer_sf <- st_as_sf(lat_longs, coords = c("lon", "lat"), crs = 4326)
  return(stock_pt_layer_sf)
}

#for use in input validation
`%then%` <- function(a, b){
  if (is.null(a)) b else a 
}

# Make the map
make_the_map <- function(data_layer){
  leaflet(data_layer) %>%
    #Basemaps
    addTiles(group = "OSM") %>%
    addMarkers(popup = ~name, group = "Company") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
    #Layer control
    addLayersControl(
      baseGroups = c("OSM", "Toner", "Toner Lite", "CartoDB Dark","CartoDB Positron","Esri Imagery"),
      overlayGroups = c("Company"),
      options = layersControlOptions(collapsed = TRUE))
}



# Define UI for my mapping application 
ui <- fluidPage(
    # Defining App theme
    theme = bs_theme(version = 5, bootswatch = "cerulean"),

    # Application title
    
    titlePanel(h1("Map Stock HQ", align ='center'),"Map Stock HQ"),

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
    
            shinycssloaders::withSpinner(  
              leafletOutput("stockmap", width = "100%", height = "75vh"),
              type = 4, hide.ui = FALSE, color = "#2fa4e7")
    )),
    
    # Adding a footer
    
)

# Define server logic required to map the data
server <- function(input, output) {
  
  company_layer <- reactive({
     #adding input validation
      validate(
        need(input$search != "", "") %then%
        need(sjmisc::is_empty(trimws(input$search)) != TRUE, "Input is an empty string") %then%
        need(try(geocode_stock(input$search)), "Urecognized stock symbol")
      )
        geocode_stock(input$search)
    
      })
  
    # output the map
    output$stockmap <- renderLeaflet({
       
        Sys.sleep(1.5)
        make_the_map(company_layer())
    
      })
    
     # proxy for dynamic changes on the map
    

}

# Run the application 
shinyApp(ui = ui, server = server)
