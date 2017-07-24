# Burgh's Eye View Parcels
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Max Cercone
library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(sp)
library(maptools)
library(raster)
library(sp)
library(maptools)
library(raster)
library(dplyr)
library(shinythemes)
library(leaflet)
library(rgdal)
library(htmltools)
library(R4CouchDB)
library(stringi)


options(scipen = 999)


dollarsComma <- function(x){
  #x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}

# Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

##Set Couch credentials
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw

# CouchDB Connection
couchDB <- cdbIni(serverName = "webhost.pittsburghpa.gov", uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-parcels")
#couchDB <- cdbIni(serverName = "webhost.pittsburghpa.gov", uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-parcels-dev")

# Determine if on mobile device
getWidth <- '$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

ckanGEO <- function(url) {
  r <- GET(url, add_headers(Authorization = "74b409d8-0f6f-439a-8a97-7796b9a0fc8b"))
  c <- content(r, as ="text")
  readOGR(c, "OGRGeoJSON", verbose = F)
}

# Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

# this_year
this_year <- format(Sys.Date(), format="%Y")

if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
  # Egg
  X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
  Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
  title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "halloween"
  load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-08")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
  load.egg <- ckan("e17e6a67-2bba-4a1a-aa36-87beb2cd0a3b")
  load.egg <- subset(load.egg, MuniName == "PITTSBURGH")
  load.egg$icon <- "election"
  load.egg$tt <- paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                        <br><b>Location: </b>", load.egg$LocName,
                        "<br><b>Ward: </b>", load.egg$Ward,
                        "<br><b>District: </b>", load.egg$District,
                        "<br><b>Address: </b>", load.egg$NewAddress,
                        '<br><center><a href="https://www.pavoterservices.state.pa.us/pages/pollingplaceinfo.aspx" target="_blank">Find your polling place!</a></center>
                        Clear the search bar to go back to the regular Burgh&#39;s Eye View!</font>'
  )
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-09"))) {
  X <- c(-79.9773187, -80.0096757, -80.0109521)
  Y <- c(40.4644031, 40.4406418, 40.4416163)
  title <- c("Herr's Island", "Fort Pitt", "Fort Duquesne")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "thanksgiving"
  load.egg$tt <- "*Gobble gobble* <br> No Results this time. Search again and have a Happy Thanksgiving!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-12-30")) | Sys.Date() <= as.Date(paste0(this_year,"-1-02"))) {
  X <- c(-80.00383, -80.003981)
  Y <- c(40.441558, 40.442340)
  title <- c("Liberty & Stanwix", "Penn & Stanwix")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "new_year"
  load.egg$tt <- "3... 2... 1... Happy New Years! <br>Looks like a fresh start to the New Year, and a fresh blank map! Try something else in the search bar!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-02-01")) & Sys.Date() <= as.Date(paste0(this_year,"-02-15"))) {
  X <-  c(-80.002398,  -80.017794, -79.964644, -79.964708, -79.983140, -79.991428)
  Y <- c(40.440397, 40.437650, 40.428210, 40.461866, 40.452217, 40.456897)
  title <- c("Market Square", "Mt. Washington", "SouthSide Works", " Church Brew Works", "The Strip", "Penn Brewery")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "valentine"
  load.egg$tt <- "Love is in the air, but doesn't look like any results are! <br>Would you be my Valentine?"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-03-01")) & Sys.Date() <= as.Date(paste0(this_year,"-03-31"))){
  X <- c(-79.9968604, -80.004055)
  Y <- c(40.4381098, 40.440631)
  title <- c("City County Building", "Market Square")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "patrick"
  load.egg$tt <- "<i>Your search didn't turn up anything, not even my Pot-o-Gold!</i>"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-04-01")) & Sys.Date() <= as.Date(paste0(this_year,"-04-30"))) {
  load.egg <- read.csv("boundaries/Parks/parks.csv")
  load.egg$icon <- "easter_egg"
  load.egg$tt <- "<i>You couldn't find any results, but maybe you can find my eggs.</i>"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-07-01")) & Sys.Date() <= as.Date(paste0(this_year,"-07-07"))) {
  load.egg <- read.csv("boundaries/Parks/parks.csv")
  load.egg$icon <- "july_4"
  load.egg$tt <- "<i>Happy Independence Day! Looks like you need to try another search term.</i>"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-05-01")) & Sys.Date() <= as.Date(paste0(this_year,"-08-31"))) {
  load.pools <- ckanGEO("https://data.wprdc.org/dataset/8186cabb-aa90-488c-b894-2d4a1b019155/resource/6f836153-ada7-4b18-b9c9-7a290c569ea9/download/pools.geojson")
  load.egg <- data.frame(coordinates(load.pools))
  colnames(load.egg) <- c("X","Y")
  load.egg$icon <- "summer"
  load.egg$tt <- "<i>Ah... Summer! Chill out, relax and grab some rays with me. Or if you'd like try another search term.</i>"
} else {
  X <- c(-79.9968604, -80.004055)
  Y <- c(40.4381098, 40.440631)
  title <- c("City County Building", "Market Square")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "snow"
  load.egg$tt <- "Burrr!! The app's not frozen, there's just nothing that fits that description here!"
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 9, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 9, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  summer = makeIcon("./icons/egg/summer.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  july_4 = makeIcon("./icons/egg/july_4.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5)               
)


hood_list <- jsonlite::fromJSON("hoods.json")

##Application
ui <- shinyUI(navbarPage(id = "navbar",
                         windowTitle = "Burgh's Eye View Parcels",
                         selected = "Parcels",
                         title = HTML('<img src="burghs_eyeview_logo_small.png" alt="Burghs Eye View" height="85%">'),
                         collapsible = TRUE,
                         position = "static-top",
                         theme = shinytheme("flatly"),
                         tabPanel(a("Points", href="https://pittsburghpa.shinyapps.io/BurghsEyeView/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                         tabPanel(a("Places", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewPlaces/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                         tabPanel("Parcels", id = "Parcels", value = "Parcels", class = "Parcels",
                                  tags$script(getWidth),
                                  # Google Tag Manager Script to Head
                                  tags$head(includeScript("tag-manager-head.js")),
                                  # Set favicon
                                  tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
                                  tags$head(HTML('<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png" />
                                                 <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76-precomposed.png" />
                                                 <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-120x120-precomposed.png" />
                                                 <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152-precomposed.png" />')),
                                  tags$head(HTML('<!-- You can use Open Graph tags to customize link previews.
                                                 Learn more: https://developers.facebook.com/docs/sharing/webmasters -->
                                                 <meta property="og:url"           content="http://www.your-domain.com/your-page.html" />
                                                 <meta property="og:type"          content="website" />
                                                 <meta property="og:title"         content="Burgh&#39;s Eye View" />
                                                 <meta property="og:description"   content="Pittsburgh&#39;s one stop shop for geographic City Data" />
                                                 <meta property="og:image"         content="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png" />')),
                                  # Add Google Analytics Script to page
                                  tags$head(includeScript("google-analytics.js")),
                                  # Add Tag Manager Script to Body
                                  tags$body(tags$noscript(tags$iframe(src='https://www.googletagmanager.com/ns.html?id=GTM-TCTCQVD', height = 0, width = 0, style="display:none;visibility:hidden"))),
                                  # Hide error codes that may appear
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"),
                                  # Background of report.table
                                  tags$style(type="text/css", '.report.table {background-color: #fff;}'),
                                  # Remove unwanted padding and margins
                                  tags$style(type="text/css", ".container-fluid {padding:0;}"),
                                  tags$style(type="text/css", ".navbar-header {margin:auto;"),
                                  tags$style(type="text/css", ".navbar-static-top {margin-bottom:0;}"),
                                  tags$style(type="text/css", ".navbar-brand {height:60px; padding:0;}"),
                                  tags$style(type="text/css", ".navbar {border-right-width: 20px;
                                             border-left-width: 65px;}"),
                                  # Set max height for pop-ups
                                  tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
                                  # Edit top bar
                                  tags$style(type= "text/css", ".form-group {
                                             margin-bottom: 0px;
                                             }"),
                                  tags$head(tags$style(type="text/css", '.Parcels {
                                                       background-image: url("loading.png");
                                                       background-repeat: no-repeat;
                                                       background-position: center;
                                                       background-size: contain;
                                                       }')),
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          # Generate layer panel & Map (checks for mobile devices)
                          uiOutput("mapPanel")
                                  ),
                         tabPanel("Data: Parcels", class = "data", value = "Data",
                                  inputPanel(
                                    uiOutput("buttonStyle")
                                  ),
                                  div(style = 'overflow-x: scroll', DT::dataTableOutput("datatable"))
                         ),
                         tabPanel('About', class = "About", value = "About",
                                  includeHTML('about.html'),
                                  # Twitter Button
                                  tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                                                   header.append('<div class =\"twit\" style=\"float:right;margin-top: 15px;\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" align=\"middle\" data-url=\"data.pittsburghpa.gov/BurghsEyeView\" data-text=\"Check out Burgh&#39;s Eye View! A new tool to view city data in Pittsburgh: https://goo.gl/z4cZ30\" data-size=\"large\">Tweet</a></div>');
                                                   console.log(header)")),
                                  tags$script(HTML("!function(d,s,id){
                                                   var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                                   if(!d.getElementById(id)){
                                                   js=d.createElement(s);
                                                   js.id=id;
                                                   js.src=p+'://platform.twitter.com/widgets.js';
                                                   fjs.parentNode.insertBefore(js,fjs);
                                                   }
                                                   }(document, 'script', 'twitter-wjs');")),
                # Facebook Button
                HTML('<div id="fb-root"></div>'),
                tags$script(HTML("(function(d, s, id) {
                                 var js, fjs = d.getElementsByTagName(s)[0];
                                 if (d.getElementById(id)) return;
                                 js = d.createElement(s); js.id = id;
                                 js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
                                 fjs.parentNode.insertBefore(js, fjs);
                                 }(document, 'script', 'facebook-jssdk'));")),
                tags$script(HTML('header.append(\'<div class="fb-share-button" style="float:right;margin-top: 15px;margin-right: 5px;" data-href="http://pittsburghpa.shinyapps.io/BurghsEyeView/?utm_source=facebook_button&amp;utm_campaign=facebook_button&amp;utm_medium=facebook%2Fsocial\" data-layout="button" data-size="large" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank" href="https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fpittsburghpa.shinyapps.io%2FBurghsEyeView%2F%23utm_source%3Dfacebook_button%26utm_campaign%3Dfacebook_button%26utm_medium%3Dfacebook%252Fsocial&amp;src=sdkpreparse">Share</a></div>\');
                                 console.log(header)'))
                )
                         ) 
                ) 




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {  
  setBookmarkExclude(c("GetScreenWidth", "datatable_rows_all"))
  #URL Bookmark 
  sessionStart <- as.numeric(Sys.time())
  names(sessionStart) <- "sessionStart"
  sessionID <- paste(stri_rand_strings(1, 5), gsub("\\.", "-", sessionStart) , "parcels", sep="-")
  names(sessionID) <- "sessionID"
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    # Connect to Couch DB
    if (length(reactiveValuesToList(input)) > 0) {
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      couchDB$dataList <- c(reactiveValuesToList(input), sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
    session$doBookmark()
  })
  
  # Update page URL
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$buttonStyle <- renderUI({
    # Generate search & layer panel & Map (checks for mobile devices)
    if (as.numeric(input$GetScreenWidth) > 800) {
      div(style="margin-top: 20px", downloadButton("downloadData", "Export Parcels", class = "dlBut"))
    } else {
      div(downloadButton("downloadData", "Export Parcels", class = "dlBut"))
    }
  })
  
  downloadInput <- reactive({
    report <- hoodinput()
    report <- report@data
    # Report Table Search Filter
    if (!is.null(input$datatable_search) && input$datatable_search != "") {
      report <- report[apply(report, 1, function(row){any(grepl(as.character(input$datatable_search), row, ignore.case = TRUE))}), ]
    }
    
    return(report)
  })
  
  # Map Tab UI
  output$mapPanel <- renderUI({  
    # UI for Desktop Users
    if (as.numeric(input$GetScreenWidth) > 800) {
      tagList(
        # Generate Map
        leafletOutput("map"),
        # Map size for Desktop CSS
        tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '300px',
          wellPanel(id = "tPanel", style = "max-height: calc(100vh - 90px) !important;",
                    # Add background image
                    tags$head(tags$style(type="text/css", '.Points {
                                         background-image: url("loading.png");
                                         background-repeat: no-repeat;
                                         background-position: center;
                                         background-size: contain;
    }')),
                    textInput("search", 
                              value = NULL,
                              label = NULL, 
                              placeholder = "Search"),
                    selectInput("neigh_select",
                                label = "Neighborhood",
                                choices = hood_list,
                                selected = "Central Business District",
                                multiple = FALSE,
                                selectize = TRUE)
                    ), style = "opacity: 0.88"
          )
        )
  } else {
    tagList(
      # Input panel for Mobile (stationary at top)
      absolutePanel(top = 65, left = 0, width = '100%' ,
                    wellPanel(id = "tPanel", style ="padding-left: 5px; padding-right: 5px;",
                              # Remove padding from Search Bar
                              tags$style(type= "text/css", "#tPanel {margin-bottom:0px; padding:0px; overflow-y:scroll; max-height: calc(100vh - 60px); !important; min-height: 55px;}"),
                              # Set background color to match panels
                              tags$style(type = "text/css", "body {background-color: #ecf0f1}"),
                              tags$style(type= "text/css", "{width:100%;
                                         margin-bottom:5px;
                                         text-align: center;}
                                         .inner
                                         {display: inline-block;}"),
                              # Div for Search Bar and Expansion
                              HTML('<div id="outer" style="position:absolute;z-index: 9; background-color:#ecf0f1; width:100%;">'),
                              # Set Searchvar width optimal for device
                              tags$style(type = "text/css", paste0('#search {width: ', input$GetScreenWidth - 84, 'px; margin-left:10px;}')),
                              # Inputs
                              div(style="display:inline-block;", 
                                  textInput("search", 
                                            value = NULL,
                                            label = NULL, 
                                            placeholder = "Search")),
                              tags$style(style="text/css", chartr0('#mapPanel button .fa:before { content: "\\f056";  }
                                                                   #mapPanel button.collapsed .fa:before { content: "\\f055";  }')),
                              HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                                   <div id="mobile" class="collapse" style="margin-top:55px;">
                                   <small style="font-size:11px;margin-left:3px">Not all locations are exact. (See &rsquo;About&rsquo; for details.)</small>
                                   <br>'),
                              selectInput("neigh_select",
                                          label = "Neighborhood",
                                          choices = hood_list,
                                          selected = "Central Business District",
                                          multiple = FALSE,
                                          selectize = TRUE),
                              HTML('</div>')
                              ),
                    # Generate Map
                    div(class="mapBack", style="position: absolute;
                        width: 100%;z-index: -1;
                        left: 0px;
                        top: 55px;", leafletOutput("map")),
                    # Set map to style for Mobile
                    tags$style(type = "text/css", "#map {height: calc(100vh - 115px) !important;}"),
                    tags$head(tags$style(type="text/css", '.mapBack {
                                         background-image: url("loading.png");
                                         background-repeat: no-repeat;
                                         background-position: center;
                                         background-size: contain;}'))
                    )
                    )
}
}
                    )  
  
  hoodinput <- reactive({
    hoodname <- gsub("\\-", "_", input$neigh_select)
    hoodname <- gsub(" ", "_", hoodname)
    hoodname <- gsub("\\.", "", hoodname)
    hoodname <- tolower(hoodname)
    url <- paste0("http://webhost.pittsburghpa.gov:5984/neighborhood_parcels/", hoodname)
    g <- GET(url, authenticate(couchdb_un, couchdb_pw))
    c <- content(g, "text")
    hood_parcel <- readOGR(c, "OGRGeoJSON", verbose = F) 
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      hood_parcel <- hood_parcel[apply(hood_parcel@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(hood_parcel)
  }
  )
  
  output$map <- renderLeaflet({ 
    hood_parcel <- hoodinput()
    
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik",
                       options = providerTileOptions(noWrap = TRUE, minZoom = 1
                       )
      ) %>%
      addLegend("topright", colors = c("#4daf4a", "#ffff33", "#e41a1c"), labels = c("Abated Properties", "City Owned", "Delinquent"))
    
    
    if(nrow(hood_parcel) > 0){ 
      print(nrow(hood_parcel))
      map <- addPolygons(map, data = hood_parcel, 
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~colorval, fillOpacity = .75,
                         popup = ~paste("<font color='black'><b>Parcel ID:</b>", paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',pin, '" target="_blank">', 
                                                                                        pin, '</a>'), 
                                        ifelse(is.na(mapblocklo), "", paste0("<br><b>Lot & Block: </b>", mapblocklo)),
                                        ifelse(is.na(ADDRESS), "", paste0("<br><b>Address: </b>", ADDRESS)), 
                                        ifelse(is.na(geo_name_nhood), "", paste0("<br><b>Neighborhood: </b>", geo_name_nhood)),
                                        ifelse(is.na(MUNIDESC), "", paste0("<br><b>Ward: </b>", MUNIDESC)),
                                        ifelse(is.na(OWNERDESC), "", paste0("<br><b>Owner Code: </b>", OWNERDESC)),
                                        ifelse(is.na(CLASSDESC), "", paste0("<br><b>Class: </b>", CLASSDESC)),
                                        ifelse(is.na(USEDESC), "", paste0("<br><b>Use Code: </b>", USEDESC)),
                                        ifelse(is.na(TAXDESC), "", paste0("<br><b>Tax Code: </b>", TAXDESC)),
                                        ifelse(is.na(SALEDATE), "", paste0("<br><b>Last Sale Date: </b>", SALEDATE)),
                                        ifelse(is.na(SALEPRICE), "", paste0("<br><b>Last Sale Price: </b>", dollarsComma(SALEPRICE))),
                                        ifelse(is.na(COUNTYLAND), "", paste0("<br><b>County Land Value: </b>", dollarsComma(COUNTYLAND))),
                                        ifelse(is.na(COUNTYBUILDING), "", paste0("<br><b>County Building Value: </b>", dollarsComma(COUNTYBUILDING))),
                                        ifelse(is.na(COUNTYTOTAL), "", paste0("<br><b>County Total Value: </b>", dollarsComma(COUNTYTOTAL))),
                                        ifelse(is.na(amount), "", paste0("<br><b>Total Lien Amount: </b>", dollarsComma(amount))),
                                        ifelse(is.na(lien_num), "", paste0("<br><b>Number of Liens: </b>", lien_num)),
                                        #"<br><b>'17 City Taxes</b>", tt_city_ta,
                                        #"<br><b>'17 School Taxes</b>", tt_school_,
                                        #"<br><b>'17 Library Taxes</b>", tt_lib_tax,
                                        #"<br><b>Current Delinquent Taxes</b>", CURRENT_DE,
                                        hood_parcel$tt,
                                        paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>')))
    } 
    if (nrow(hood_parcel) == 0) {
      if (Sys.Date() >= as.Date(paste0(this_year,"-11-01")) & Sys.Date() <= as.Date(paste0(this_year,"-11-08"))) {
        egg <- load.egg
      } else {
        egg <- load.egg[sample(1:nrow(load.egg),1),]
      }
      map <- addMarkers(map, data=egg, ~X, ~Y, icon = ~icons_egg[icon], popup = ~tt) %>% 
        setView(-79.9959, 40.4406, zoom = 10)
    }
    map
  }) 
  
  ##Data Table
  output$datatable <- DT::renderDataTable({
    hood_parcel <- hoodinput()
    hood_parcel@data
    hood_parcel@data <- subset(hood_parcel@data, select = c("pin", "mapblocklo", "ADDRESS", "PROPERTYZIP", "geo_name_nhood", "MUNIDESC", "TAXDESC",
                                                            "USEDESC", "OWNERDESC", "CLASSDESC", "YEARBLT", "SALEDATE", "SALEPRICE",
                                                            "COUNTYLAND", "COUNTYBUILDING", "COUNTYTOTAL", "delq", "cityown", "tt"))
    colnames(hood_parcel@data) <- c("Parcel ID", "Lot & Block", "Address", "Zip", "Neighborhood", "Ward", "Tax Code", "Use Code", "Owner Code", "Class", "Year Built",
                                    "Last Sale Date", "Last Sale Price", "County Land Value", "County Building Value", "County Total Value", "Delinquent", "City Owned", "Abatements")
    hood_parcel@data
    
  }, options = list(pageLength = 10,
                    dom = "Bfrtip",
                    lengthMenu = c(10,20, 30),
                    #scrollX = TRUE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#95a5a6'});",
                      "}"),
                    searchHighlight = TRUE), 
  class = 'cell-border stripe',
  rownames = FALSE,
  escape = FALSE
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$neigh_select, ".csv") },
    content = function(file) {
      write.csv(downloadInput(), file)
    }
  )
  
  })  

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url") 