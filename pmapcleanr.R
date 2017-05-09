##Cleaner file for public parcel map
## 4/28/2017

library(jsonlite)
library(httr)
library(dplyr)
library(plyr)
library(lubridate)
library(R4CouchDB)
library(geojsonio)
library(maptools)
library(rgdal)

set_config(config(ssl_verifypeer = 0L))

options(scipen = 999)

dollarsComma <- function(x){
  x <- prettyNum(x, big.mark = ",")
  paste0("$", x)
}

##Set Couch credentials
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw

##Set this year variable
this_year <- as.Date(format(Sys.Date(), format="%Y-01-01"))


##Query Ckan API for Property Assessment Data
query <- "https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22PARID%22%2C%22PROPERTYHOUSENUM%22%2C%22PROPERTYFRACTION%22%2C%22PROPERTYADDRESS%22%2C%22PROPERTYZIP%22%2C%22MUNIDESC%22%2C%22TAXDESC%22%2C%22CLASSDESC%22%2C%22OWNERDESC%22%2C%22USEDESC%22%2C%22HOMESTEADFLAG%22%2C%22COUNTYLAND%22%2C%22COUNTYBUILDING%22%2C%22COUNTYTOTAL%22%2C%22SALEPRICE%22%2C%22SALEDATE%22%2C%22YEARBLT%22%20from%20%22518b583f-7cc8-4f60-94d0-174cc98310dc%22%20WHERE%20%22SCHOOLCODE%22%20LIKE%20%2747%27&limit=9999999"
getdata <- GET(url=query)
assessment <- jsonlite::fromJSON(content(getdata, "text"))
assessment <- assessment$result$records
assessment$MUNIDESC <- as.factor(assessment$MUNIDESC)
assessment <- subset(assessment, MUNIDESC != "Mt. Oliver  ")

##Query Ckan API for Delinquent
delinquent.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=ed0d1550-c300-4114-865c-82dc7c23235b&limit=99999"
getdelqdata <- GET(url=delinquent.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
delq <- jsonlite::fromJSON(content(getdelqdata, "text"))
delq <- delq$result$records
assessment$delq <- ifelse(assessment$PARID %in% delq$pin, TRUE, FALSE)

##Query Ckan API for City owned property
ownership.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=4ff5eb17-e2ad-4818-97c4-8f91fc6b6396&limit=99999"
getownerdata <- GET(url=ownership.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
ownership <- jsonlite::fromJSON(content(getownerdata, "text"))
ownership <- ownership$result$records
assessment$cityown <- ifelse(assessment$PARID %in% ownership$pin, TRUE, FALSE)


##Query Ckan API for Property Tax Abatements
abatement.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=fd924520-d568-4da2-967c-60b3a305e681&limit=99999"
getabatedata <- GET(url=abatement.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
abatement <- jsonlite::fromJSON(content(getabatedata, "text"))
abatement <- abatement$result$records
abatement <- subset(abatement, select = c("pin", "program_name", "start_year", "num_years", "abatement_amt"))
abatement$abatement <- TRUE

##Lien Data
liens <- read.csv("liens.csv")

##Neighborhood
system('python pghhoods.py')
load.nhood <- jsonlite::fromJSON("./pghnhoods.txt")
load.nhood <- load.nhood$result$records
load.nhood$geo_name_nhood <- as.factor(load.nhood$geo_name_nhood)

##Create neighborhood JSON list
hood_list <- as.list(levels(load.nhood$geo_name_nhood))
hood_list <- gsub("\\-", "_", hood_list)
hood_list <- gsub(" ", "_", hood_list)
hood_list <- gsub("\\.", "", hood_list)
hood_list <- tolower(hood_list)
write_json(hood_list, "hoodlist.json")

##Merge all datasets together
parcels <- merge(assessment, abatement, by.x = "PARID", "pin", all.x = TRUE)
parcels$ADDRESS <- paste(parcels$PROPERTYHOUSENUM, parcels$PROPERTYADDRESS)
parcels$SALEDATE <- gsub("-", "/", parcels$SALEDATE)
parcels$SALEDATE <- as.Date(parcels$SALEDATE, "%m/%d/%Y")
parcels.liens <- merge(parcels, liens, by.x = "PARID", by.y = "pin", all.x = TRUE)
parcels.hoods <- merge(parcels.liens, load.nhood, by.x = "PARID", by.y = "PIN", all.x = TRUE)
parcels.hoods$nhood <- gsub("\\-", "_", parcels.hoods$geo_name_nhood)
parcels.hoods$nhood <- gsub(" ", "_", parcels.hoods$nhood)
parcels.hoods$nhood <- gsub("\\.", "", parcels.hoods$nhood)
parcels.hoods$nhood <- tolower(parcels.hoods$nhood)
##General Cleaning
parcels.hoods$abatement_amt[is.na(parcels.hoods$abatement_amt)] <- 0
parcels.hoods$amount[is.na(parcels.hoods$amount)] <- 0
parcels.hoods$owedto[is.na(parcels.hoods$owed)] <- 0
parcels.hoods$program_name[is.na(parcels.hoods$program_name)] <- "No Abatement"
parcels.hoods$abatement[is.na(parcels.hoods$abatement)] <- FALSE


##Set colors for map
parcels.hoods$colorval <- "#ccc7c7"
parcels.hoods$colorval <- ifelse(parcels.hoods$program_name == "No Abatement", parcels.hoods$colorval, "#4daf4a")
parcels.hoods$colorval <- ifelse(parcels.hoods$cityown == TRUE, "#ffff33", parcels.hoods$colorval)
#parcels.hoods$colorval <- ifelse(parcels.hoods$tif == TRUE, "#f781bf", parcels.hoods$colorval)
#parcels.hoods$colorval <- ifelse(parcels.hoods$USEDESC == "VACANT LAND", "#a65628", parcels.hoods$colorval)
parcels.hoods$colorval <- ifelse(parcels.hoods$delq == TRUE, "#e41a1c", parcels.hoods$colorval)
parcels.final <- subset(parcels.hoods, nhood != "")
parcels.final$nhood <- as.factor(parcels.final$nhood)
colnames(parcels.final)[1] <- "pin"

##Function that binds geoJSON data to dataframe
baseURL <- "http://tools.wprdc.org/geoservice/parcels_in/pittsburgh_neighborhood/"
for (i in levels(parcels.final$nhood)[54:90]){ 
  r <- GET(paste0(baseURL,i, "/")) 
  f <- content(r, "text", encoding = "ISO-8859-1")
  org <- readOGR(f, "OGRGeoJSON", verbose = F)
  ##Final add data
  org@data <- merge(org@data, parcels.final, by = "pin", all.x = TRUE, sort = FALSE)
  #Couch DB Function posting to DB goes here
  ##CouchDB Connection
  couchDB <- cdbIni(serverName = "webhost.pittsburghpa.gov", uname = couchdb_un, pwd = couchdb_pw, DBName = "neighborhood_parcels")
  couchDB$dataList <- (org)
  couchDB$id <- i
  cdbAddDoc(couchDB)
  print(paste(i, "completed"))
}

