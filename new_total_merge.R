##Cleaning/Merge File for Parcel Viewer
##Last update 2/22/2017

library(plyr)
library(dplyr)
library(sp)
library(maptools)
library(raster)
library(sp)
library(maptools)
library(raster)
library(dplyr)


dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}
options(scipen = 999)

##Merge Total Parcel csv generated from REALESTATE to WPRDC parcel/assessment data
system('python test.re.py')
load.new_parcel <- read.csv("./test_city_parcels.csv")
centroids <- read.csv("./august_parcelcentroids.csv")
load.new_parcel$CURRENT_DELQ[is.na(load.new_parcel$CURRENT_DELQ)] <- 0
centroids <- subset(centroids, geo_name_cousub == "Pittsburgh city")
new_merged <- merge(load.new_parcel, centroids, by.x = "COUNTY_PIN", by.y = "PIN", all = TRUE)
new_merged <- subset(new_merged, geo_name_nhood != 'MOUNT OLIVER BOROUGH')
new_merged <- subset(new_merged, select = c('COUNTY_PIN', 'CITY_PIN', 'OWNER', 'ADDRESS', 'PROP_ZIP', 'LAST_SALE_DATE', 'BANKRUPT_FLAG',
                                               'HOMESTEAD', 'CURRENT_DELQ', 'PROGRAM_NAME', 'ABATEMENT_LENGTH', 'START_YEAR', 'APPROVED_USER',
                                               'MAPBLOCKLO', 'geo_name_nhood', 'Pgh_Ward', "Pgh_CityCouncil2012", 'x', 'y'))

city_assessed <- read.csv('./city_assessed.csv')
new_merged <- merge(new_merged, city_assessed, by.x = "COUNTY_PIN", by.y = "PARID", all.x = TRUE)
new_merged <- subset(new_merged, select = c("CITY_PIN", "COUNTY_PIN", "OWNER", "ADDRESS", "PROP_ZIP", "geo_name_nhood", "Pgh_Ward", "Pgh_CityCouncil2012", "MAPBLOCKLO", 
                                            "SALEDATE", "SALEPRICE", "COUNTYLAND", "COUNTYBUILDING", "COUNTYTOTAL", "MUNIDESC", "TAXDESC", "OWNERDESC", "CLASSDESC", "USEDESC",
                                            "CURRENT_DELQ", "PROGRAM_NAME", "ABATEMENT_LENGTH", "START_YEAR", "APPROVED_USER", "x", "y", "HOMESTEAD"))
##test for public owned
load.public_propz <- read.csv("./public_propz.csv")
load.public_propz <- subset(load.public_propz, select = c("PARCEL_NUMBER", "SALEPRICE", "CNTY_OWNER_NAME"))
load.public_propz$SALEPRICE[is.na(load.public_propz$SALEPRICE)] <- 0
load.public_propz <- subset(load.public_propz, SALEPRICE == 0)
load.public_propz <- subset(load.public_propz, CNTY_OWNER_NAME == "CITY OF PITTSBURGH" | CNTY_OWNER_NAME == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH")
#load.public_propz$PUBLIC_SALE <- TRUE
load.public_propz <- subset(load.public_propz, select = c("PARCEL_NUMBER"))
new_merged <- merge(new_merged, load.public_propz, by.y = "PARCEL_NUMBER", by.x = "COUNTY_PIN", all.x = TRUE)



##Drop 12 missing records and change column names
new_merged <- new_merged[-c(1:12), ]
names(new_merged)[names(new_merged) == "CNTY_OWNER_NAME"] <- "OWNER"
names(new_merged)[names(new_merged) == "geo_name_nhood"] <- "Neighborhood"
names(new_merged)[names(new_merged) == "Pgh_Ward"] <- "Ward"
names(new_merged)[names(new_merged) == "Pgh_CityCouncil2012"] <- "Council_Distr"


##Load in prior year delinquency
prior_delq <- read.csv("./jts_delq.csv")
prior_delq$delq <- TRUE
names(prior_delq)[names(prior_delq) == "CNTY_ACCT"] <- "ID"
prior_delq <- subset(prior_delq, select = c("ID", "delq"))
new_merged <- merge(new_merged, prior_delq, by.y = "ID", by.x = "COUNTY_PIN", all.x = TRUE)
new_merged$delq <- ifelse(is.na(new_merged$delq), FALSE, new_merged$delq)
new_merged$delq <- ifelse(new_merged$TAXDESC == "10 - Exempt", FALSE, new_merged$delq)



##Calculate taxes owed by using millage from RE, construct tool tip amounts with dollars, account for tax exemption
load.millage <- read.csv('./millage.csv')
id.max <- as.numeric(max(load.millage$M_YEAR))
load.millage <- subset(load.millage, M_YEAR == id.max)
new_merged$city_tax <- ifelse(new_merged$HOMESTEAD == "H" | new_merged$HOMESTEAD == "7" | new_merged$HOMESTEAD == "4", (new_merged$COUNTYTOTAL - load.millage$CITY_HOMESTEAD)*load.millage$CITY_LAND, 
                              (new_merged$COUNTYTOTAL * load.millage$CITY_LAND))
new_merged$school_tax <- ifelse(new_merged$HOMESTEAD == "H" | new_merged$HOMESTEAD == "7" | new_merged$HOMESTEAD == "4", (new_merged$COUNTYTOTAL - load.millage$SCHOOL_HOMESTEAD)*load.millage$SCHOOL_LAND, 
                                (new_merged$COUNTYTOTAL * load.millage$SCHOOL_LAND))
new_merged$lib_tax <- ifelse(new_merged$HOMESTEAD == "H" | new_merged$HOMESTEAD == "7" | new_merged$HOMESTEAD == "4", (new_merged$COUNTYTOTAL - load.millage$CITY_HOMESTEAD)*load.millage$LIBRARY_LAND, 
                             (new_merged$COUNTYTOTAL * load.millage$LIBRARY_LAND))
new_merged$city_tax <- ifelse(new_merged$TAXDESC == "10 - Exempt" | new_merged$TAXDESC == "12 - Public Utility Realty Tax - PURTA", 0, new_merged$city_tax)
new_merged$school_tax <- ifelse(new_merged$TAXDESC == "10 - Exempt" | new_merged$TAXDESC == "12 - Public Utility Realty Tax - PURTA", 0, new_merged$school_tax)
new_merged$lib_tax <- ifelse(new_merged$TAXDESC == "10 - Exempt" | new_merged$TAXDESC == "12 - Public Utility Realty Tax - PURTA", 0, new_merged$lib_tax)
new_merged$tt_city_tax <- dollarsComma(new_merged$city_tax)
new_merged$tt_school_tax <- dollarsComma(new_merged$school_tax)
new_merged$tt_lib_tax <- dollarsComma(new_merged$lib_tax)
new_merged$tt_SALE_PRICE <- dollarsComma(new_merged$SALEPRICE)
new_merged$tt_COUNTYBUILDING <- dollarsComma(new_merged$COUNTYBUILDING)
new_merged$tt_COUNTYLAND <- dollarsComma(new_merged$COUNTYLAND)
new_merged$tt_COUNTYTOTAL <- dollarsComma(new_merged$COUNTYTOTAL)



##Edit tax abatement information
new_merged$PROGRAM_NAME <- as.character(new_merged$PROGRAM_NAME)
new_merged$PROGRAM_NAME[new_merged$PROGRAM_NAME==""] <- "NA"
new_merged$PROGRAM_NAME <- as.factor(new_merged$PROGRAM_NAME)
new_merged$APPROVED_USER <- as.character(new_merged$APPROVED_USER)
new_merged$APPROVED_USER[new_merged$APPROVED_USER==""] <- "NA"
new_merged$APPROVED_USER <- as.factor(new_merged$APPROVED_USER)

##Map city region based on municipal description

new_merged <- transform(new_merged, REGION = as.factor(mapvalues(MUNIDESC, c("1st Ward  - PITTSBURGH", "2nd Ward - PITTSBURGH", "3rd Ward - PITTSBURGH", "4th Ward - PITTSBURGH",
                                                                         "5th Ward - PITTSBURGH", "6th Ward - PITTSBURGH", "7th Ward - PITTSBURGH", "8th Ward - PITTSBURGH",
                                                                         "9th Ward - PITTSBURGH", "10th Ward - PITTSBURGH", "11th Ward - PITTSBURGH", "12th Ward - PITTSBURGH",
                                                                         "13th Ward - PITTSBURGH", "14th Ward - PITTSBURGH", "15th Ward - PITTSBURGH", "16th Ward - PITTSBURGH",
                                                                         "17th Ward - PITTSBURGH", "18th Ward - PITTSBURGH", "19th Ward - PITTSBURGH", "20th Ward - PITTSBURGH",
                                                                         "21st Ward - PITTSBURGH", "22nd Ward - PITTSBURGH", "23rd Ward - PITTSBURGH", "24th Ward - PITTSBURGH",
                                                                         "25th Ward - PITTSBURGH", "26th Ward - PITTSBURGH", "27th Ward - PITTSBURGH", "28th Ward - PITTSBURGH",
                                                                         "29th Ward - PITTSBURGH", "30th Ward - PITTSBURGH", "31st Ward - PITTSBURGH", "32nd Ward - PITTSBURGH"),
                                                             c("East End", "East End", "East End", "East End", "East End", "East End", "East End",
                                                               "East End", "East End", "East End", "East End", "East End", "East End", "East End", "East End",
                                                               "South Hills/Side", "South Hills/Side", "South Hills/Side", "South Hills/Side", "West End",
                                                               "North Side", "North Side", "North Side", "North Side", "North Side", "North Side", "North Side",
                                                               "West End", "South Hills/Side", "South Hills/Side", "South Hills/Side", "South Hills/Side"))))

##Load in TIF data and create binary field for color purposes
load.tif <- read.csv('./tif_prop.csv')
load.tif$tif <- TRUE
load.tif <- subset(load.tif, select = c("PIN", "tif"))
new_merged <- merge(new_merged, load.tif, by.y = "PIN", by.x = "COUNTY_PIN", all.x = TRUE)
new_merged$tif <- ifelse(is.na(new_merged$tif), FALSE, new_merged$tif)

##Create URL to Allegheny County Real Estate site via County Parcel Identifier
new_merged$URL <- paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',new_merged$COUNTY_PIN, '" target="_blank">', 
                              new_merged$COUNTY_PIN, '</a>')

##Load lien data
load.liens <- read.csv("./liens.csv")
new_merged <- merge(new_merged, load.liens, by.x = "COUNTY_PIN", by.y = "pin", all.x = TRUE)
new_merged$amount[is.na(new_merged$amount)] <- 0
new_merged$owedto[is.na(new_merged$owedto)] <- 0
new_merged$tt_amount <- dollarsComma(new_merged$amount)


write.csv(new_merged, './all_city_parcels.csv')



city_parcels <- read.csv('./all_city_parcels.csv')

parcels <- readShapeSpatial("allegheny_county_parcel_boundaries.shp")
parcel.2 <- merge(parcels, city_parcels, by.x = "pin", by.y = "COUNTY_PIN", duplicateGeoms = TRUE)
parcel.2 <- subset(parcel.2, !is.na(parcel.2$y))

##Add conditional colors to parcels (delinquent, abatement, ownership)
parcel.2$Neighborhood <- as.character(parcel.2$Neighborhood)
parcel.2$Neighborhood <- as.factor(parcel.2$Neighborhood)
parcel.2$PROGRAM_NAME <- as.character(parcel.2$PROGRAM_NAME)
parcel.2$PROGRAM_NAME[parcel.2$PROGRAM_NAME == ""] <- NA
parcel.2$PROGRAM_NAME[is.na(parcel.2$PROGRAM_NAME)] <- "No Abatement"
parcel.2$color_val <- "#ccc7c7"
parcel.2$color_val <- ifelse(parcel.2$PROGRAM_NAME == "No Abatement", parcel.2$color_val, "#4daf4a")
parcel.2$color_val <- ifelse(parcel.2$OWNER == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH", "#984ea3", parcel.2$color_val)
parcel.2$color_val <- ifelse(parcel.2$OWNER == "CITY OF PITTSBURGH", "#ffff33", parcel.2$color_val)
parcel.2$color_val <- ifelse(parcel.2$tif == TRUE, "#f781bf", parcel.2$color_val)
#parcel.2$color_val <- ifelse(parcel.2$USEDESC == "VACANT LAND", "#a65628", parcel.2$color_val)
parcel.2$color_val <- ifelse(parcel.2$delq == TRUE, "#e41a1c", parcel.2$color_val)
parcel.2$color_val <- ifelse(is.na(parcel.2$OWNER), "#ccc7c7", parcel.2$color_val)

names(parcel.2)[names(parcel.2) == "amount"] <- "lien"

writeSpatialShape(parcel.2, "/home/linadmin/WPRDC/Cartegraph/outputWPRDC/city_parcels")
##Subset City shp into four regional spatialpolygondataframe
ee_parcel <- subset(parcel.2, REGION == "East End")
we_parcel <- subset(parcel.2, REGION == "West End")
ns_parcel <- subset(parcel.2, REGION == "North Side")
sh_parcel <- subset(parcel.2, REGION == "South Hills/Side")

##Write four regional shp
writeSpatialShape(ee_parcel, "east_end_parcels")
writeSpatialShape(we_parcel, "west_end_parcels")
writeSpatialShape(ns_parcel, "north_side_parcels")
writeSpatialShape(sh_parcel, "south_hill_parcels")

