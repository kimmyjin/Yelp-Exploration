#########################################data###########
library(dplyr)
coord <- yelp_data$coordinate %>% unlist %>% matrix(ncol=2,byrow = TRUE)
yelp_data <- yelp_data %>% mutate(lat = coord[,1]) %>% mutate(long = coord[,2])
#######################################shiny####
library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(ggplot2)


shinyApp(
  ui = fluidPage(
    titlePanel("Yelp"),
    sidebarLayout(
      sidebarPanel(
        selectInput("city","select a city",c("",sort(table$City))),          
        hr(),
        selectInput("category","Category",c("Rating"="rating","Review Count"="review_count")),
        hr(),
        plotOutput("plot1")
      ),
      mainPanel(
        leafletOutput('Map'),
        tags$div(id="cite",
                 'Data from', tags$em('Wikipedia'), 'and',tags$em('Yelp')
        ),
        plotOutput("plot2")
      )
    )
  ),
  server = function(input, output,session) {
    ###data
    yelp_data1 <- reactive({
      city1 <- input$city
      if (city1 == ""){
        yelp_data%>% select(city,lat,long) %>% mutate(city = unlist(city)) %>% 
          group_by(city) %>% mutate(lat = mean(lat),long = mean(long),name = city) %>%
          unique() %>% as_data_frame()
      }else{
          yelp_data %>% filter(city == city1)
          # select(name,long,lat) %>% 
          # group_by(long,lat) %>% mutate(name = list(name)) %>%unique()
        
          #test1 = lapply(test$name,unlist)
      }
    })
      
    ###Map
      output$Map <- renderLeaflet({
        if(ncol(yelp_data1())==4){
          zo <- 4
          lng1 <- -93.85
          lat1 <- 37.45
        }else{
          zo <-12
          lng1 <- mean(yelp_data1()$long)
          lat1 <- mean(yelp_data1()$lat)
        }
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          addMarkers(
            lng = yelp_data1()$long, lat = yelp_data1()$lat, 
            popup=yelp_data1()$name)%>%
          setView(lng = lng1, lat = lat1, zoom = zo)
  })
  ###plot
      # output$plot2 <- renderPlot({
      #   # If no city are in view, don't plot
      #   if (input$city == ""){
      #     return(NULL)
      #   }else{
      #     plotdata = yelp_data1() %>% select(name,rating,review_count) %>%
      #       mutate(pop = unlist(rating)*unlist(review_count))
      # 
      #     # hist(pop,freq = FALSE,main=paste("The Popularity of" ,input$city,"Restaurant"))
      #     ggplot(data = plotdata, aes(x = name, y = pop)) +
      #       geom_bar(stat="identity")
      #   }
      # })
      
      
  }
)