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
        plotOutput("pieplot")
      ),
      mainPanel(
        leafletOutput('Map'),
        tags$div(id="cite",
                 'Data from', tags$em('Wikipedia'), 'and',tags$em('Yelp')
        ),
        plotOutput("plot2")
        
        
        #br(),
        #plotOutput("pieplot")
        
        
      )
    )
  ),
  server = function(input, output,session) {
   
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
        return(category)
    })
    
     output$pieplot=renderPlot({
       ggplot(df1(),aes(x="",y=count1,fill=V1))+geom_bar(width=1,stat="identity")+
         coord_polar("y",start=0)+
         theme_bw()+
         facet_grid(city~.,scales="free")+
         ylab("")
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
          select(name,long,lat) %>% 
          group_by(long,lat) %>% mutate(name = list(name)) %>% unique()
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
        
        content <- rep(paste(sep = "<br/>",
                             "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                             "606 5th Ave. S",
                             "Seattle, WA 98138"),13)
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          addMarkers(yelp_data1$long, lat = yelp_data1$lat,  popup =cont,
                     options = popupOptions(closeButton = TRUE)
          )%>%
          setView(lng = lng1, lat = lat1, zoom = zo)
      }
    })
    
    
    
    
  }
)