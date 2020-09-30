library(readr)
library(sp)
library(rgdal)
library(dplyr)
library(shiny)
library(tmap)
library(leaflet)


listing.df <- read_csv('./app_data/listings_clean.csv', col_types = cols())

hood.geo <- readOGR(dsn = './app_data/NTA Boundaries.geojson', verbose = FALSE)

rate.fields <- c('neighbourhood_group_cleansed', 'neighbourhood_cleansed', 'property_type', 'room_type', 
                 'bedrooms', 'bathrooms', 'guests_included', 'price')
rate.df <- na.omit(listing.df[, rate.fields])

rate.df$neighbourhood_group_cleansed <- as.factor(rate.df$neighbourhood_group_cleansed)
rate.df$neighbourhood_cleansed <- as.factor(rate.df$neighbourhood_cleansed)
rate.df$property_type <- as.factor(rate.df$property_type)
rate.df$room_type <- as.factor(rate.df$room_type)
rate.df$bedrooms <- as.integer(rate.df$bedrooms)
rate.df$bathrooms <- as.numeric(rate.df$bathrooms)
rate.df$guests_included <- as.integer(rate.df$guests_included)
rate.df$price <- as.numeric(rate.df$price)

rate.df <- rate.df[!apply(rate.df[, c('price')], 1, function(x) any(x == 0)), ]

rate.df$lnprice <- log(rate.df$price)

lnprice.lq <- quantile(rate.df$lnprice, probs = 0.25)
lnprice.uq <- quantile(rate.df$lnprice, probs = 0.75)
lnprice.iqr <- 1.5 * (lnprice.uq - lnprice.lq)
rate.df <- rate.df[between(rate.df$lnprice, lnprice.lq - lnprice.iqr, lnprice.uq + lnprice.iqr), ]

rate.df <- rate.df[rate.df$neighbourhood_cleansed %in% 
                     names(which(table(rate.df$neighbourhood_cleansed) >= 5)), ]
rate.df$neighbourhood_cleansed <- as.factor(as.character(rate.df$neighbourhood_cleansed))

rate.df <- rate.df[rate.df$property_type %in% 
                     names(which(table(rate.df$property_type) >= 100)), ]
rate.df$property_type <- as.factor(as.character(rate.df$property_type))

rate.df$neighbourhood_group_cleansed <- relevel(rate.df$neighbourhood_group_cleansed, 'Manhattan')
rate.df$neighbourhood_cleansed <- relevel(rate.df$neighbourhood_cleansed, 'Harlem')
rate.df$property_type <- relevel(rate.df$property_type, 'Apartment')
rate.df$room_type <- relevel(rate.df$room_type, 'Entire home/apt')

rate.linr <- lm(lnprice ~ neighbourhood_cleansed + property_type + room_type + bedrooms + 
                  bathrooms  + guests_included, 
                data = rate.df, na.action = na.exclude)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'borough', label = 'Borough: ',
                  choices = sort(levels(rate.df$neighbourhood_group_cleansed)), selected = 'Manhattan'),
      uiOutput('bhood'),
      selectInput(inputId = 'prop', label = 'Property Type: ', 
                  choices = sort(levels(rate.df$property_type)), selected = 'Apartment'),
      uiOutput('proprm'),
      selectInput(inputId = 'bed', label = 'Number of Bedrooms: ', 
                  choices = seq(0, 10, 1), selected = 1),
      selectInput(inputId = 'bath', label = 'Number of Bathrooms: ',
                  choices = seq(1.0, 7.0, 0.5), selected = 1),
      selectInput(inputId = 'guest', label = 'Number of Guests Included: ',
                  choices = seq(1, 15, 1), selected = 1)
    ),
    mainPanel(leafletOutput(outputId= 'map', width = '100%', height = '600')
    )
  )
)


server <- function(input, output) {
  
  output$bhood <- renderUI({
    selectInput(inputId = 'hood', label = 'Neighbourhood: ', 
                choices = rate.df %>%
                  filter(neighbourhood_group_cleansed == input$borough) %>%
                  select(neighbourhood_cleansed) %>%
                  unique() %>%
                  arrange(neighbourhood_cleansed),
                selected = 'Harlem')
  })
  
  output$proprm <- renderUI({
    selectInput(inputId = 'room', label = 'Room Type: ', 
                choices = rate.df %>%
                  filter(property_type == input$prop) %>%
                  select(room_type) %>%
                  unique() %>%
                  arrange(room_type),
                selected = 'Entire home/apt')
  })
  
  output$map <- renderLeaflet({
    res.df <- data.frame('neighbourhood_cleansed' = input$hood, 
                         'property_type' = input$prop,
                         'room_type' = input$room, 
                         'bedrooms' = as.integer(input$bed),
                         'bathrooms' = as.numeric(input$bath), 
                         'guests_included' = as.integer(input$guest))
    res.pred.df <- predict(rate.linr, newdata = res.df, na.action = na.pass, interval = 'prediction')
    res.pred.df <- round(exp(res.pred.df), 2)
    res.df <- cbind(res.df, res.pred.df)
    res.df$text <- paste0('Rate: $', res.df$fit, ' | Lower 2.5%: $', res.df$lwr, 
                          ' | Upper 97.5%: $' , res.df$upr)
    res.geo <- hood.geo[hood.geo$neighbourhood == input$hood, ]
    res.geo <- merge(res.geo, res.df, by.x = 'neighbourhood', by.y = 'neighbourhood_cleansed')
    tmap_leaflet(tm_shape(res.geo) +
                   tm_fill(id = 'neighbourhood', col = 'darkseagreen', alpha = 0.7, n = 6) +
                   tm_borders() + 
                   tm_text(text = 'text', size = 1.2, col = 'black') +
                   tm_layout(title = paste0('Predicted Rate (1-Night) for Airbnb rentals in ',
                                            res.geo$neighbourhood)))
  })
  
}


shinyApp(ui = ui, server = server, options = list(height = 600))