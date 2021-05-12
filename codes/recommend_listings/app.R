library(readr)
library(sp)
library(rgdal)
library(dplyr)
library(rgeos)
library(reshape2)
library(shiny)
library(tmap)
library(leaflet)
library(DT)

listing.df <- read_csv('./app_data/listings_clean.csv', col_types = cols())

hood.geo <- readOGR(dsn = './app_data/NTA Boundaries.geojson', verbose = FALSE)

recommend.fields <- c('listing_url', 'neighbourhood_group_cleansed', 'neighbourhood_cleansed', 
                      'latitude', 'longitude', 'property_type', 'room_type', 'bedrooms', 'bathrooms', 
                      'guests_included', 'price')
recommend.df <- na.omit(listing.df[, recommend.fields])

recommend.df$listing_url <- as.character(recommend.df$listing_url)
recommend.df$neighbourhood_cleansed <- as.factor(recommend.df$neighbourhood_cleansed)
recommend.df$neighbourhood_group_cleansed <- as.factor(recommend.df$neighbourhood_group_cleansed)
recommend.df$latitude <- as.numeric(recommend.df$latitude)
recommend.df$longitude <- as.numeric(recommend.df$longitude)
recommend.df$property_type <- as.factor(recommend.df$property_type)
recommend.df$room_type <- as.factor(recommend.df$room_type)
recommend.df$bedrooms <- as.integer(recommend.df$bedrooms)
recommend.df$bathrooms <- as.numeric(recommend.df$bathrooms)
recommend.df$guests_included <- as.integer(gsub('[^0-9.]', '', as.character(recommend.df$guests_included)))
recommend.df$price <- as.numeric(gsub('[^0-9.]', '', as.character(recommend.df$price)))
recommend.df$price_guest <- round(recommend.df$price/recommend.df$guests_included, 2)
recommend.df$listing_url <- paste0('<a target = "_blank", href="', recommend.df$listing_url, '">', 
                                   recommend.df$price, '</a>')


ui <- fluidPage(
  tags$style(HTML('table.dataTable tr.selected td, 
                  table.dataTable td.selected {background-color: #fff !important;},
                  .nowrap {white-space: nowrap;}')),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'borough', label = 'Borough: ',
                  choices = sort(levels(recommend.df$neighbourhood_group_cleansed)), 
                  selected = 'Manhattan'),
      uiOutput('bhood'),
      selectInput(inputId = 'bed', label = 'Number of Bedrooms: ', 
                  choices = seq(0, 10, 1), selected = 1),
      selectInput(inputId = 'guest', label = 'Number of Guests Included: ',
                  choices = seq(1, 15, 1), selected = 1),
      numericInput(inputId = 'price', label = 'Budget : ',
                   value = 50, min = 1, max = 10000),
      width = 4
    ),
    mainPanel(
      leafletOutput(outputId= 'map', width = '100%', height = '500'), width = 8
    )
  ),
  dataTableOutput(outputId = 'recdf', width = '100%', height = '500')
)


server <- function(input, output) {

  output$bhood <- renderUI({
    selectInput(inputId = 'hood', label = 'Neighbourhood: ', 
                choices = recommend.df %>%
                  filter(neighbourhood_group_cleansed == input$borough) %>%
                  select(neighbourhood_cleansed) %>%
                  unique() %>%
                  arrange(neighbourhood_cleansed), 
                selected = 'Harlem')
  })  
  
  recommend.geo <- reactive({
    recommend.geo <- recommend.df %>% filter(neighbourhood_group_cleansed == input$borough)
    coordinates(recommend.geo) <- ~longitude+latitude
    
    search.pt <- coordinates(hood.geo[hood.geo$neighbourhood == input$hood, ])
    search.pt.geo <- data.frame('listing_url' = NA,
                                'neighbourhood_group_cleansed' = input$borough,
                                'neighbourhood_cleansed' = input$hood,
                                'longitude' = search.pt[1], 'latitude' = search.pt[2],
                                'property_type' = NA, 'room_type' = NA, 
                                'bedrooms' = as.integer(input$bed), 'bathrooms' = NA,
                                'guests_included' = as.integer(input$guest),
                                'price' = as.numeric(input$price),
                                'price_guest' = as.numeric(input$price)/as.integer(input$guest))
    coordinates(search.pt.geo) <- ~longitude+latitude
    
    recommend.geo$rank <- rank(gDistance(search.pt.geo, recommend.geo, byid = TRUE))
    recommend.geo <- recommend.geo[recommend.geo$rank <= 100, ]
    recommend.geo@data <- recommend.geo@data[!names(recommend.geo@data) %in% c('rank')]
    
    recommend.geo <- rbind(search.pt.geo, recommend.geo)
    
    idx <- recommend.geo@data %>%
      mutate(lnprice_guest = log(price_guest)) %>%
      select(bedrooms, guests_included, lnprice_guest) %>%
      sapply(scale) %>%
      dist(method = 'euclidean') %>%
      as.matrix() %>%
      melt(varnames = c('row', 'col')) %>%
      filter(value != 0.0 & col == 1) %>%
      arrange(value) %>%
      select(row)
    
    recommend.geo <- recommend.geo[idx$row,][1:10,]
    proj4string(recommend.geo) <- CRS(proj4string(hood.geo))
    
    rownames(recommend.geo@data) <- NULL
    recommend.geo$weight <- rev(as.integer(rownames(recommend.geo@data)))
    recommend.geo$text <- paste0('$', recommend.geo$price, ' per Night')
    
    recommend.geo
  })
  
  output$map <- renderLeaflet({
    tmap_leaflet(tm_shape(recommend.geo()) + 
                   tm_dots(col = 'property_type', size = 'weight', alpha = 0.9, palette = 'Dark2', 
                           title = 'Property Type', id = 'text',
                           popup.vars=c('NTA:' = 'neighbourhood_cleansed',
                                        'Room Type:' = 'room_type', 
                                        '# Bedrooms:' = 'bedrooms',
                                        '# Bathrooms:' = 'bathrooms',
                                        '# Guests:' = 'guests_included')) + 
                   tm_basemap('Esri.WorldTopoMap') + 
                   tm_layout(title = 'Recommended Airbnb Listings'))
  })
  
  output$recdf <- renderDataTable({
    datatable(recommend.geo()@data[c('neighbourhood_cleansed', 'property_type', 'room_type', 'bedrooms',
                                     'bathrooms', 'guests_included', 'listing_url')],
              colnames = c('Neighbourhood', 'Property-Type', 'Room-Type', '# of Bedrooms', 
                           '# of Bathrooms', '# of Guests', 'Price'),
              escape = FALSE)
  })
}


shinyApp(ui = ui, server = server, options = list(height = 1000))
