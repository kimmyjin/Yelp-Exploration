library(dplyr)
library(parallel)
library(rvest)
library(httr)
library(httpuv)
library(jsonlite)

consumerKey = "KfzIQYbCbnzktysQDoQL-Q"
consumerSecret = "pKaj2a2wP-D8_DuG5HCmgqezmgw"
token = "JFjn4UqrKEnIb8MyaqPyYVH3__yTTSd8"
token_secret = "Nl2CL8OIJ9L4jXaaW5PMWMaDn-Y"

zip_url = "http://www.unitedstateszipcodes.org/"
stateName = zip_url %>% read_html() %>% html_nodes(".state-links a") %>% html_text()

abr = tolower(state.abb[match(stateName,state.name)])
abr[49] = "dc"
abr =  na.omit(abr)

state_url = paste0(zip_url, abr, "/")
zipcode = list()

for (i in seq_along(state_url)) {
  zipcode[[i]] = state_url[i] %>% read_html() %>% html_nodes("td:nth-child(1) a") %>% html_text()
}

thin_zip = sapply(1:length(state_url), function(i) sample(zipcode[[i]], 0.02*length(zipcode[[i]])))
zip = unlist(thin_zip)

# authorization
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

yelpurl = paste0("http://api.yelp.com/v2/search/?limit=20&term=restaurants&sort=2&location=", zip)

getDf = function(url) {
  locationdata = GET(url, sig)
  locationdataContent = content(locationdata)
  if (length(jsonlite::fromJSON(toJSON(locationdataContent$businesses))) != 0) {
    location = jsonlite::fromJSON(toJSON(locationdataContent$businesses[[1]]$location))
    locaDf = data.frame(t(sapply(location,c))) %>%
      select(city, address, coordinate, state_code)
    locationdataList = jsonlite::fromJSON(toJSON(locationdataContent))
    busiDf = locationdataList$businesses %>% 
      select(rating, review_count, name, categories, phone) 
  }
  return(unique(cbind(busiDf, locaDf)))
} 
listDf = mclapply(yelpurl, getDf, mc.cores=24)
yelp_data_2perc = bind_rows(listDf)

save(yelp_data_2perc, file ="yelp2%.RData")