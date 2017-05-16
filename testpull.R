library(rgdal)
library(jsonlite)
library(httr)
library(dplyr)
library(plyr)
library(lubridate)
library(geojsonio)

hoodname <- jsonlite::fromJSON("hoods.json")
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw

url <- paste0("http://webhost.pittsburghpa.gov:5984/", i, "/_design/parcels/_view/FeatureCollection")
g1 <- GET(paste0(url), authenticate(couchdb_un, couchdb_pw), add_headers(`Content-Type`= "application/json"))
c1 <- content(g1, "text")
c <- content(g1)$rows
j <- as.character(toJSON(c))
pi <- readOGR(j, layer = "OGRGeoJSON", verbose = F, drop_unsupported_fields = TRUE, dropNULLGeometries = TRUE)  
 



url <- paste0("http://webhost.pittsburghpa.gov:5984/", i, "/") 
g <- GET(paste0(url, k), authenticate(couchdb_un, couchdb_pw)) 
c <- content(g, "text") 
p <- readOGR(c, "OGRGeoJSON", verbose = F) 
