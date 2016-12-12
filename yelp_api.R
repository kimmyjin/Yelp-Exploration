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

offset = 10
offset = seq(from = 0, to = 480, by = 40)

yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=40&location=SanFrancisco%20IL&offset=",offset,"")

getDf = function(url) {
  locationdata = GET(url, sig)
  locationdataContent = content(locationdata)
  locationdataList = jsonlite::fromJSON(toJSON(locationdataContent))
  busi = locationdataList$businesses %>% 
    select(rating, review_count, name, categories, phone, location, distance)
  loca = locationdataList$businesses %>% select(location)
  # locat = loca %>% select(city, address, postal_code, coordinate, state_code)
  return(cbind(busi, locat))
} 

locationdata = GET(yelpurl, sig)
locationdataContent = content(locationdata)


locationdataContent$businesses[[1]]$location

lo = jsonlite::fromJSON(toJSON(locationdataContent$businesses[[1]]$location)) %>%
  as.data.frame()


locationdataList = jsonlite::fromJSON(toJSON(locationdataContent))
busi = locationdataList$businesses %>% select(location)

dim(busi)
class(busi$location)


listDf = lapply(yelpurl, getDf)
busi_data = bind_rows(listDf)

names(listDf[[1]]$location)
View(listDf[[1]])
View(listDf[[2]])

dim(data.frame(locationdataList))
View(data.frame(locationdataList))
