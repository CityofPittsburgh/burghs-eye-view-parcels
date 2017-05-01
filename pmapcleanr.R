##Cleaner file for public parcel map
## 4/28/2017


library(jsonlite)
library(httr)
library(dplyr)
library(plyr)
library(lubridate)

options(scipen = 999)

dollarsComma <- function(x){
  x <- prettyNum(x, big.mark = ",")
  paste0("$", x)
}

##Query Ckan API for Property Assessment Data
query <- "https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20from%20%22518b583f-7cc8-4f60-94d0-174cc98310dc%22%20WHERE%20%22SCHOOLCODE%22%20LIKE%20%2747%27&limit=9999999"
getdata <- GET(url=query)
assessment <- jsonlite::fromJSON(content(getdata, "text"))
assessment <- assessment$result$records
assessed <- subset(assessment, select = c("PARID", "PROPERTYHOUSENUM", "PROPERTYFRACTION", "PROPERTYADDRESS", "PROPERTYZIP", "MUNIDESC", "TAXDESC", "CLASSDESC", "OWNERDESC",
                                          "USEDESC", "HOMESTEADFLAG", "COUNTYLAND", "COUNTYBUILDING", "COUNTYTOTAL", "SALEPRICE", "SALEDATE", "YEARBLT"))
assessed$MUNIDESC <- as.factor(assessed$MUNIDESC)
assessed <- subset(assessed, MUNIDESC != "Mt. Oliver  ")

##Query Ckan API for Delinquent
delinquent.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=ed0d1550-c300-4114-865c-82dc7c23235b&limit=99999"
getdelqdata <- GET(url=delinquent.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
delq <- jsonlite::fromJSON(content(getdelqdata, "text"))
delq <- delq$result$records
delq <- subset(delq, select = c("pin", "current_delq", "prior_years"))
delq$delq <- TRUE

##Query Ckan API for City owned property
ownership.query <- "https://data.wprdc.org/api/action/datastore_search?resource_id=4ff5eb17-e2ad-4818-97c4-8f91fc6b6396&limit=99999"
getownerdata <- GET(url=ownership.query, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
ownership <- jsonlite::fromJSON(content(getownerdata, "text"))
ownership <- ownership$result$records
ownership <- subset(ownership, select = c("pin"))
ownership$cityowned <- TRUE

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
load.nhood <- fromJSON("./pghnhoods.txt")
load.nhood <- load.nhood$result$records
load.nhood$geo_name_nhood <- as.factor(load.nhood$geo_name_nhood)

##Create neighborhood JSON list
hood_list <- as.list(levels(load.nhood$geo_name_nhood))
hood_list <- gsub("\\-", "_", hood_list)
hood_list <- gsub(" ", "_", hood_list)
hood_list <- tolower(hood_list)
write_json(hood_list, "hoodlist.json")

##Merge all datasets together
all.property <- merge(assessed, delq, by.x = "PARID", by.y = "pin", all.x = TRUE)
all.property <- merge(all.property, ownership, by.x = "PARID", "pin", all.x = TRUE)
all.property <- merge(all.property, abatement, by.x = "PARID", "pin", all.x = TRUE)
all.property$ADDRESS <- paste(all.property$PROPERTYHOUSENUM, all.property$PROPERTYADDRESS)
all.property$SALEDATE <- gsub("-", "/", all.property$SALEDATE)
all.property$SALEDATE <- as.Date(all.property$SALEDATE, "%m/%d/%Y")
all.property <- merge(all.property, liens, by.x = "PARID", by.y = "pin", all.x = TRUE)
all.property <- merge(all.property, load.nhood, by.x = "PARID", by.y = "PIN", all.x = TRUE)
all.property$nhood <- gsub("\\-", "_", all.property$geo_name_nhood)
all.property$nhood <- gsub(" ", "_", all.property$nhood)
all.property$nhood <- tolower(all.property$nhood)
##General Cleaning
all.property$current_delq <- as.numeric(all.property$current_delq)
all.property$current_delq[is.na(all.property$current_delq)] <- 0
all.property$abatement_amt[is.na(all.property$abatement_amt)] <- 0
all.property$amount[is.na(all.property$amount)] <- 0
all.property$owedto[is.na(all.property$owed)] <- 0
all.property$program_name[is.na(all.property$program_name)] <- "No Abatement"
all.property$abatement[is.na(all.property$abatement)] <- FALSE
all.property$delq[is.na(all.property$delq)] <- FALSE
all.property$cityowned[is.na(all.property$cityowned)] <- FALSE

#Set tooltip fields with dollars and commas
all.property$tt_land <- dollarsComma(all.property$COUNTYLAND)
all.property$tt_build <- dollarsComma(all.property$COUNTYBUILDING)
all.property$tt_total <- dollarsComma(all.property$COUNTYTOTAL)
all.property$tt_sale <- dollarsComma(all.property$SALEPRICE)
all.property$tt_delq <- dollarsComma(all.property$current_delq)
all.property$tt_abatement <- dollarsComma(all.property$abatement_amt)
all.property$tt_lienamt <- dollarsComma(all.property$amount)

##Set colors for map
all.property$colorval <- "#ccc7c7"
all.property$colorval <- ifelse(all.property$program_name == "No Abatement", all.property$colorval, "#4daf4a")
all.property$colorval <- ifelse(all.property$cityowned == TRUE, "#ffff33", all.property$colorval)
#all.property$colorval <- ifelse(all.property$tif == TRUE, "#f781bf", all.property$colorval)
#all.property$colorval <- ifelse(all.property$USEDESC == "VACANT LAND", "#a65628", all.property$colorval)
all.property$colorval <- ifelse(all.property$delq == TRUE, "#e41a1c", all.property$colorval)

##Function that binds geoJSON data to dataframe

