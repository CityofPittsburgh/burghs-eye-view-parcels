##Cleaner file for public parcel map
## 4/28/2017

library(jsonlite)
library(httr)
library(dplyr)
library(plyr)
library(lubridate)
library(geojsonio)
library(maptools)
library(rgdal)
library(R4CouchDB)
library(sp)

set_config(config(ssl_verifypeer = 0L))

options(scipen = 999)

dollarsComma <- function(x){
  x <- prettyNum(x, big.mark = ",")
  paste0("$", x)
}

##Set Couch credentials
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw

##Query Ckan API for Property Assessment Data
query <- "https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22PARID%22%2C%22PROPERTYHOUSENUM%22%2C%22PROPERTYFRACTION%22%2C%22PROPERTYADDRESS%22%2C%22PROPERTYZIP%22%2C%22MUNIDESC%22%2C%22TAXDESC%22%2C%22CLASSDESC%22%2C%22OWNERDESC%22%2C%22USEDESC%22%2C%22HOMESTEADFLAG%22%2C%22COUNTYLAND%22%2C%22COUNTYBUILDING%22%2C%22COUNTYTOTAL%22%2C%22SALEPRICE%22%2C%22SALEDATE%22%2C%22YEARBLT%22%20from%20%22518b583f-7cc8-4f60-94d0-174cc98310dc%22%20WHERE%20%22SCHOOLCODE%22%20LIKE%20%2747%27&limit=9999999"
getdata <- GET(url=query, timeout(60))
assessment <- jsonlite::fromJSON(content(getdata, "text"))
assessment <- assessment$result$records
assessment$MUNIDESC <- as.factor(assessment$MUNIDESC)
assessment <- subset(assessment, MUNIDESC != "Mt. Oliver  ")

##Query Ckan API for Delinquent
delinquent.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=ed0d1550-c300-4114-865c-82dc7c23235b&limit=99999"
getdelqdata <- GET(url=delinquent.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"), timeout(60))
delq <- jsonlite::fromJSON(content(getdelqdata, "text"))
delq <- delq$result$records
assessment$delq <- ifelse(assessment$PARID %in% delq$pin, TRUE, FALSE)

##Query Ckan API for City owned property
ownership.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=4ff5eb17-e2ad-4818-97c4-8f91fc6b6396&limit=99999"
getownerdata <- GET(url=ownership.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"), timeout(60))
ownership <- jsonlite::fromJSON(content(getownerdata, "text"))
ownership <- ownership$result$records
assessment$cityown <- ifelse(assessment$PARID %in% ownership$pin, TRUE, FALSE)


##Query Ckan API for Property Tax Abatements
abatement.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=fd924520-d568-4da2-967c-60b3a305e681&limit=99999"
getabatedata <- GET(url=abatement.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"), timeout(60))
abatement <- jsonlite::fromJSON(content(getabatedata, "text"))
abatement <- abatement$result$records
abatement <- subset(abatement, select = c("pin", "program_name", "start_year", "num_years", "abatement_amt"))
abatement$abatement <- TRUE
abatement$tool <- paste0("<dt>", abatement$program_name, ":", abatement$num_years, "</dt>", "<dd>", abatement$start_year, ":", abatement$abatement_amt, "/<dd>")


##Lien Data
#system('python pittsburghliens.py')
liens <- read.csv("liens.csv")

##Neighborhood
#system('python pghhoods.py')
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
#parcels <- merge(assessment, abatement, by.x = "PARID", "pin", all.x = TRUE)
assessment$ADDRESS <- paste(assessment$PROPERTYHOUSENUM, assessment$PROPERTYADDRESS)
assessment$SALEDATE <- gsub("-", "/", assessment$SALEDATE)
assessment$SALEDATE <- as.Date(assessment$SALEDATE, "%m/%d/%Y")
parcels.liens <- merge(assessment, liens, by.x = "PARID", by.y = "pin", all.x = TRUE)
parcels.hoods <- merge(parcels.liens, load.nhood, by.x = "PARID", by.y = "PIN", all.x = TRUE)
parcels.hoods$nhood <- gsub("\\-", "_", parcels.hoods$geo_name_nhood)
parcels.hoods$nhood <- gsub(" ", "_", parcels.hoods$nhood)
parcels.hoods$nhood <- gsub("\\.", "", parcels.hoods$nhood)
parcels.hoods$nhood <- tolower(parcels.hoods$nhood)
##General Cleaning
#parcels.hoods$abatement_amt[is.na(parcels.hoods$abatement_amt)] <- 0
#parcels.hoods$amount[is.na(parcels.hoods$amount)] <- 0
parcels.hoods$owedto[is.na(parcels.hoods$owed)] <- 0
#parcels.hoods$program_name[is.na(parcels.hoods$program_name)] <- "No Abatement"
#parcels.hoods$abatement[is.na(parcels.hoods$abatement)] <- FALSE
colnames(parcels.hoods)[1] <- "pin"

##Append abatements
abt <- abatement
abt <- abt[abt$pin %in% parcels.hoods$pin,]
abt$pin <- as.factor(abt$pin)
# Loop which aggregates appropriate abatements
for (i in levels(abt$pin)){
  # Isolate abatements for parcel IDS
  temp <- subset(abt, pin == i)
  # Sort abatements to order by data
  temp <- temp[order(temp$start_year),]
  # Isolate only tooltip
  temp <- temp[,"tool"]
  # Create DT string from list
  tt <- paste0('<br><b>Abatement:</b><br><dl style="margin-bottom: 0px; margin-left:10px";>', toString(temp), "</dl>")
  # Remove junk characters from unlisting
  tt <- gsub(",", "", tt)
  tt <- gsub('" "', "", tt)
  tt <- gsub('c\\("', "", tt)
  tt <- gsub('"\\)', "", tt)
  # Create column for bind
  df <- data.frame(i, tt)
  # Check for first abatement
  if (i == levels(abt$pin)[1]){
    tt.df <- df
    # Merge to other tooltips
  }else {
    tt.df <- rbind(tt.df, df)
  }
}

# Rename columns for merge
colnames(tt.df) <- c("pin", "tt")
# Merge Abatement Tooltip to Parcel DF
parcels.hoods <- merge(parcels.hoods, tt.df, by = "pin", all.x = TRUE)
parcels.hoods$tt <- as.character(parcels.hoods$tt)
parcels.hoods$tt[is.na(parcels.hoods$tt)] <- ""


##Set colors for map
parcels.hoods$colorval <- "#ccc7c7"
parcels.hoods$colorval <- ifelse(parcels.hoods$tt != "", "#4daf4a", parcels.hoods$colorval)
parcels.hoods$colorval <- ifelse(parcels.hoods$cityown == TRUE, "#ffff33", parcels.hoods$colorval)
parcels.hoods$colorval <- ifelse(parcels.hoods$delq == TRUE, "#e41a1c", parcels.hoods$colorval)
parcels.final <- subset(parcels.hoods, nhood != "")
parcels.final$nhood <- as.factor(parcels.final$nhood)
parcels.final$pin <- as.character(parcels.final$pin)


##Function that binds geoJSON data to dataframe making each neighborhood a database
#baseURL <- "http://tools.wprdc.org/geoservice/parcels_in/pittsburgh_neighborhood/"
#setwd("./neighborhoodparcels")
#for (i in levels(parcels.final$nhood)){ 
#  r <- GET(paste0(baseURL,i, "/")) 
#  f <- content(r, "text", encoding = "ISO-8859-1")
#  org <- readOGR(f, "OGRGeoJSON", verbose = F)
#  # Final add data
#  org@data <- merge(org@data, parcels.final, by = "pin", sort = FALSE)
#  org@data$pin <- as.character(org@data$pin)
#  org <- org["mapblocklo" != "Not Assessed" | "mapblocklo" != "COMMON GROUND",]
#  for (k in unique(org@data$pin)){
#    sub.1 <- subset(org, pin != "")
#   sub <- subset(org, k == pin)
#    writeOGR(sub, "parcel", layer="meuse", driver="GeoJSON")
#    g.org <- readr::read_lines("parcel")
#    # PUT Function posting to DB goes here
#    url <- paste0("http://webhost.pittsburghpa.gov:5984/", i, "/")
#    p <- PUT(paste0(url, k), body = g.org, authenticate(couchdb_un, couchdb_pw), verbose())
#    print(paste(i, k, "completed", sep="-"))
#  }
#}    


##Create neighborhood databases in CouchDB
#for (i in levels(parcels.final$nhood)){ 
#  couchDB <- cdbIni(serverName = "webhost.pittsburghpa.gov", uname = couchdb_un, pwd = couchdb_pw, newDBName = i)
#  couchDB$newDBName = i
#  couchDB <- cdbMakeDB(couchDB)
#  print(paste(i, "completed"))
#} 


##Creates documents in Neighborhood Parcels' database containing all nhood parcels
##Url from WPRDC's geojson API
setwd("./neighborhoodparcels")
for (i in levels(parcels.final$nhood)){ 
  r <- GET(paste0("http://tools.wprdc.org/geoservice/parcels_in/pittsburgh_neighborhood/" ,i, "/"), timeout(60)) 
  f <- content(r, "text", encoding = "ISO-8859-1")
  #read content of API call as geojson file
  org <- readOGR(f, "OGRGeoJSON", verbose = F)
  ##Final add data
  hood <- subset(parcels.final, nhood == i)
  org$pin <- as.character(org$pin)
  org$rowname <- 1:nrow(org)
  org <- sp::merge(org, parcels.final, by = "pin", all.x = TRUE, duplicateGeoms = TRUE)
  org@data <- org@data[order(org@data$rowname),]
  org <- org[!org@data$mapblocklo %in% c("Not Assessed", "COMMON GROUND"),]
  writeOGR(org, i, layer="meuse", driver="GeoJSON")
  j.org <- fromJSON(i)
  #call CouchDB to get last revision ID
  rurl <- paste0("http://webhost.pittsburghpa.gov:5984/neighborhood_parcels/", i)
  rg <- GET(rurl, authenticate(couchdb_un, couchdb_pw), timeout(60))
  rev <- rg$header$etag
  rev <- gsub('\"', "", rev)
  #Add _revid to JSON for CouchDB updates and PUT updates to Couch docs
  j.org$`_rev` <- as.character(rev)
  write(jsonlite::toJSON(j.org, digits = NA, pretty = TRUE, auto_unbox = TRUE), file =  paste0(i, ".json")) 
  b <- readr::read_lines(paste0(i, ".json"))
  PUT(paste0("http://webhost.pittsburghpa.gov:5984/neighborhood_parcels/", i), body = b, authenticate(couchdb_un, couchdb_pw), verbose())
  print(paste(i, "complete"))
}