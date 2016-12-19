tab = unique(map_chr(yelp_data$categories,1))
city = unique(table$City)
count_p= NULL
DF = NULL 
  #matrix(NA, nrow = 200, ncol = 3)
for(j in 1:20){
  data = yelp_data[which(yelp_data$city==city[j]),]
  tab = unique(map_chr(data$categories,1))
  category=NULL
  for(i in seq_along(tab)){
    # data = yelp_data[which(yelp_data$city==city[j]),]
    # tab = unique(map_chr(data$categories,1))
    #DF[,1] = rep(city[j], 10)
    pattern = agrep(tab[i], data$categories)
    count1 = length(pattern)
    #count_p = c(count_p, count1)
    temp = as.data.frame(cbind(tab[i],count1))  
    category=rbind(category,temp)
    #category$count_p = as.integer(as.vector(category$count_p))
    #category = head(category[order(category$count_p, decreasing = TRUE), ],10)
  }
  category$count1 = as.integer(as.vector(category$count1))
  #category = category[order(category$count_p, decreasing = TRUE), ]
  category = head(category[order(category$count1, decreasing = TRUE), ],10)
  category$city = rep(city[j], 10)
  DF = rbind(DF, category)
}
library(ggplot2)
ny=DF[1:40,]

ggplot(ny,aes(x="",y=count1,fill=V1))+geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  theme_bw()+
  facet_grid(city~.,scales="free")+
  ylab("")
  


category = as.data.frame(cbind(tab,count_p))  
category$count_p = as.integer(as.vector(category$count_p))
category = head(category[order(category$count_p, decreasing = TRUE), ],10)






