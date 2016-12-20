#loading the packages
library(dplyr)
library(parallel)
require(httr)
require(httpuv)
require(jsonlite)

#authorization step
#setting variables for the validation of scrapping
consumerKey = "-PPitwpvzIKTO4kzMT_u5g"
consumerSecret = "5LSxS2M4ht4Xq-J7pSTjtt879Do"
token = "cgZvBMDN6UnT-KPLrfeD0gX0rTNR18ls"
token_secret = "cKlcQEUDE6PW_4SVA69fEQ11Z_A"

#connecting to yelp database
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

#Number of business results to return
limit = 40

#Offsetting the list of returned business results by this amount
offset = seq(from = 0, to = 480, by = 40)

#defining a paste function which returning a combined url
pasteFun = function(a, b){
  return(paste0("http://api.yelp.com/v2/search/?limit=40&term=restaurant&location=",a,"&offset=",b))
}

#using outer function to get all of urls
yelpurl = outer(City_name, offset, pasteFun) 

#setting all urls as a vector
yelpurl = as.vector(t(yelpurl))

#defining a function to get all desired information from the yelp api and saving them as a dataframe
getDf = function(url) {
  #getting the location data of url
  locationdata = GET(url, sig)
  locationdataContent = content(locationdata)
  #getting the detailed address of restaurant
  location = jsonlite::fromJSON(toJSON(locationdataContent$businesses[[1]]$location))
  #choosing "city", "postal_code", "address", "coordinate" and "state_code" columns
  locaDf = data.frame(t(sapply(location,c))) %>%
    select(city, postal_code, address, coordinate, state_code)
  locationdataList = jsonlite::fromJSON(toJSON(locationdataContent))
  #choosing "rating", "review_count", "name", "categories", and "phone" columns
  busiDf = locationdataList$businesses %>% 
    select(rating, review_count, name, categories, phone)
  #combining two dataframe together
  return(cbind(busiDf, locaDf))
} 

#using the mclappy with 24 cores to reduce running time
listDf = mclapply(yelpurl, getDf, mc.cores=24)
#getting yelp_data dataframe by combining all rows 
yelp_data = bind_rows(listDf)

#saving the data
save(yelp_data,
     file = "yelp.RData")

