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

limit <- 40

# 10 bars in Chicago
yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&location=SanFrancisco%20IL&offset=60")
# or 10 bars by geo-coordinates
yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&ll=37.659789,-122.431026&term=bar&offset=500")

locationdata=GET(yelpurl, sig)
locationdataContent = content(locationdata)
locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))

dim(data.frame(locationdataList))
View(data.frame(locationdataList))
