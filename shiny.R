library(shiny)
library(leaflet)
library(dplyr)


shinyApp(
  ui = fluidPage(
    titlePanel("Yelp"),
        sidebarLayout(
        sidebarPanel(
          selectInput("city","select a city",sort(table$City)),          
          hr(),
          selectInput("category","Category",NULL),
          hr(),
          plotOutput("plot1"),
          plotOutput("plot2")
      ),
      mainPanel(
        leafletOutput('Map'),
        tags$div(id="cite",
                 'Data from', tags$em('Wikipedia'), 'and',tags$em('Yelp')
        )
      )
    )
  ),
  server = function(input, output) {
    output$Map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
  }
)