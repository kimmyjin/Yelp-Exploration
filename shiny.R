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
library(purrr)


shinyApp(
  ui = fluidPage(
    titlePanel("Yelp"),
    sidebarLayout(
      sidebarPanel(
        selectInput("city","select a city",c("",sort(table$City))),          
        hr(),
        checkboxGroupInput("checkGroup1", label = h5("Circle Property"),  #Create check box for mean, median and confidence level
                           choices = list("Rating (opacity)"="rating","Review Count (radius)"="review_count"),inline=TRUE),
        hr(),
        plotOutput("pieplot")
      ),
      mainPanel(
        textOutput("text1"),
        hr(),
        leafletOutput('Map'),
        tags$div(id="cite",
                 'Data from', tags$em('Wikipedia'), 'and',tags$em('Yelp')
        )
      )
    )
  ),
  server = function(input, output,session) {
    ###plot
    df1=reactive({
      DF = NULL 
      data = yelp_data[which(yelp_data$city==input$city),]
      tab = unique(map_chr(data$categories,1))
      category=NULL
      for(i in seq_along(tab)){
        pattern = agrep(tab[i], data$categories)
        count1 = length(pattern)
        temp = as.data.frame(cbind(tab[i],count1))  
        category=rbind(category,temp)
      }
      category$count1 = as.integer(as.vector(category$count1))
      category = head(category[order(category$count1, decreasing = TRUE), ],10)
      category$city = rep(input$city, 10)
      names(category)=c("type","count","city")
      return(category)
    })
    
    output$pieplot=renderPlot({
      if (input$city == ""){
        return(NULL)
      }else{
        ggplot(df1(),aes(x="",y=count,fill=type))+geom_bar(width=1,stat="identity")+
          coord_polar("y",start=0)+
          theme_bw()
      }
    })
    
    ###Map
    output$Map <- renderLeaflet({
      
      city1 <- input$city
      if (city1 == ""){
        
        yelp_data1 <- yelp_data%>% select(city,lat,long) %>% mutate(city = unlist(city)) %>% 
          group_by(city) %>% mutate(lat = mean(lat),long = mean(long),name = city) %>%
          unique() %>% as_data_frame()
        
        zo <- 4
        lng1 <- -93.85
        lat1 <- 37.45
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          addMarkers(
            lng = yelp_data1$long, lat = yelp_data1$lat, 
            popup=yelp_data1$name)%>%
          setView(lng = lng1, lat = lat1, zoom = zo)
        
      }else{
        
        yelp_data1 = yelp_data %>% 
          filter(city == city1)%>%
          select(name,long,lat,rating,review_count) %>% 
          group_by(long,lat) %>% 
          mutate(name = list(name),rating=mean(as.numeric(rating)),review_count=mean(as.numeric(review_count))) %>% 
          unique()
        
        rad = mean(yelp_data1$review_count)
        alp = 0.5
        
        if (any(input$checkGroup1=="rating")){
          alp = (yelp_data1$rating/max(yelp_data1$rating))^10
        }
        # generate median values if median is selected
        if (any(input$checkGroup1=="review_count")){
          rad = yelp_data1$review_count*2
        }
        
        cont <- NULL
        for (i in 1:nrow(yelp_data1)){
          yelp_data2 = yelp_data1$name[[i]]
          cont1 <- NULL
          for (j in 1:length(yelp_data2)){
            cont1 = paste( sep = "<br/>",cont1,yelp_data2[[j]])
          }
          cont <- c(cont,cont1)
        }
        
        zo <-12
        lng1 <- mean(yelp_data1$long)
        lat1 <- mean(yelp_data1$lat)
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          addCircles(yelp_data1$long, lat = yelp_data1$lat,stroke = FALSE,
                     color = "#FF0000",
                     radius = rad,  opacity = alp,fillOpacity =alp,
                     popup =cont,
                     options = popupOptions(closeButton = TRUE)
          )%>%
          setView(lng = lng1, lat = lat1, zoom = zo)
      }
    })
    
###instruction
    output$text1 <- renderText({
      if (input$city == ""){
      "Please click pinpoints to see city names"
      }else{
        "Please click circles to see the local best match restaurants"
      }
    })
  }
)