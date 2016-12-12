library(dplyr)

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
  return(paste0("http://api.yelp.com/v2/search/?limit=40&location=",a,"&offset=",b))
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
listDf = lapply(yelpurl, getDf)
yelp_data = bind_rows(listDf)

save(yelp_data,
     file = "yelp.RData")

