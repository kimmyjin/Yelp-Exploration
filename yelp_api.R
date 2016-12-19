library(dplyr)
library(parallel)
consumerKey = "-PPitwpvzIKTO4kzMT_u5g"
consumerSecret = "5LSxS2M4ht4Xq-J7pSTjtt879Do"
token = "cgZvBMDN6UnT-KPLrfeD0gX0rTNR18ls"
token_secret = "cKlcQEUDE6PW_4SVA69fEQ11Z_A"

require(httr)
require(httpuv)
require(jsonlite)
# authorization
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)
limit = 40
offset = seq(from = 0, to = 480, by = 40)

pasteFun = function(a, b){
  return(paste0("http://api.yelp.com/v2/search/?limit=40&term=restaurants&location=",a,"&offset=",b))
}
yelpurl = outer(City_name, offset,pasteFun) 
yelpurl = as.vector(t(yelpurl))


getDf = function(url) {
  locationdata = GET(url, sig)
  locationdataContent = content(locationdata)
  location = jsonlite::fromJSON(toJSON(locationdataContent$businesses[[1]]$location))
  locaDf = data.frame(t(sapply(location,c))) %>%
    select(city, postal_code, address, coordinate, state_code)
  locationdataList = jsonlite::fromJSON(toJSON(locationdataContent))
  busiDf = locationdataList$businesses %>% 
    select(rating, review_count, name, categories, phone)
  return(cbind(busiDf, locaDf))
} 
listDf = mclapply(yelpurl, getDf, mc.cores=24)
yelp_data = bind_rows(listDf)

save(yelp_data,
     file = "yelp.RData")
aa = yelp_data$categories %>% flatten(.)
bb = aa%>% flatten(.)

yelp_data$categories =sub(x = yelp_data$categories,"(.\\()(.*)(\\))",replace ="\\2")
yelp_data$address=sub(x = yelp_data$address,"(.\\()(.*)(\\))",replace ="\\2")
yelp_data$coordinate=sub(x = yelp_data$coordinate,"(.*\\()(.*)(\\))",replace ="\\2")
