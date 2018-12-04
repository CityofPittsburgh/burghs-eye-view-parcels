# Burgh's Eye View Parcels
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Maxwell Cercone
# Maintained by: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)

#"Dogfooding" Packages
library(httr)
library(jsonlite)
library(R4CouchDB)

# Visuals Libraries
library(leaflet)
library(rgeos)
library(rgdal)
library(sf)
library(geojsonio)
library(DT)
library(sp)

# Data Transform
library(plyr)
library(dplyr)
library(stringi)
library(lubridate)
library(tidyr)

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
couchdb_url <- jsonlite::fromJSON("key.json")$couchdb_url

# CouchDB Connection
# couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-parcels")
couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-parcels-dev")

# Determine if on mobile device
getWidth <- '$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

# Get ID's
getIds <- function(phrase) {
  url <- paste0("http://data.wprdc.org/api/action/package_search?q=", gsub(" ", "%20", phrase))
  r <- GET(url)
  raw <- content(r, "text")
  df <- jsonlite::fromJSON(raw)$result$results
  tib <- tibble(df$resources) %>%
    unnest() %>%
    filter(format == "CSV")
  final <- df %>%
    select(id, name) %>%
    right_join(tib, by = c("id" = "package_id"))
  
  return(final)
}

ckanSQL <- function(url) {
  r <- RETRY("GET", url)
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

# this_year
this_year <- format(Sys.Date(), format="%Y")

# Presidential Years
presidential_years <- seq(2016, 3000, 4)

# Election Day
nov <- ymd(as.Date(paste0(this_year, "-11-01")))
dow <- sapply(seq(0,7),function(x) format(nov+x, "%a"))
eDay <- nov + which(dow=="Mon")[1]

# Primary Day
if (this_year %in% presidential_years) {
  april <- ymd(as.Date(paste0(this_year, "-04-01")))
  dow <- sapply(seq(0,7),function(x) format(april+x, "%a"))
  firstTuesday <- april + which(dow=="Tue")[1]
  # In Presidential Years PA Primaries are on the 4th Tuesday of April
  pDay <- firstTuesday + 20
} else {
  may <- ymd(as.Date(paste0(this_year, "-05-01")))
  dow <- sapply(seq(0,7),function(x) format(may+x, "%a"))
  firstTuesday <- may + which(dow=="Tue")[1]
  # In Non-Presidential Years PA Primaries are on the 3rd Tuesay of May
  pDay <- firstTuesday + 13
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 31, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5, iconWidth = 72),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 31, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  summer = makeIcon("./icons/egg/summer.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  july_4 = makeIcon("./icons/egg/july_4.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72)      
)

hood_list <- jsonlite::fromJSON("hoods.json")

##Application
ui <- function(request) {
              navbarPage(id = "navbar",
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
                                  # Notification Centered and Color Fix
                                  tags$head(tags$style(HTML('.shiny-notification {
                                                            position: fixed;
                                                            background: #2c3e50;
                                                            top: calc(50%);;
                                                            left: calc(50%);;
                                                            width: calc(25%);;
                                                            min-width: 200px;
                                                            transform: translate(-50%, 0);}
                                                            .loading:after {
                                                              overflow: hidden;
                                                              display: inline-block;
                                                              vertical-align: bottom;
                                                              -webkit-animation: ellipsis steps(4,end) 900ms infinite;      
                                                              animation: ellipsis steps(4,end) 900ms infinite;
                                                              content: "...";
                                                              width: 0px;
                                                            }
                                                            @keyframes ellipsis {
                                                              to {
                                                                width: 1.25em;    
                                                              }
                                                            }
                                                            @-webkit-keyframes ellipsis {
                                                              to {
                                                                width: 1.25em;    
                                                              }
                                                            }'))),
                                  # Layout CSS
                                  tags$style(type="text/css", ".shiny-output-error { visibility: hidden;}
                                                               .shiny-output-error:before { visibility: hidden; }
                                                               .container-fluid { padding:0; }
                                                               .navbar-header {margin:auto;}
                                                               .navbar-static-top {margin-bottom:0;}
                                                               .navbar-brand {height:60px; 
                                                                              padding:0;}
                                                               .navbar {border-right-width: 20px;
                                                                        border-left-width: 65px;}
                                                               .leaflet-popup-content {overflow-y: auto; 
                                                                                       max-height: 400px !important;}
                                                               .form-group {margin-bottom: 0px;}
                                                               @media only screen and (min-width: 600px) {
                                                                 #map {height: calc(100vh - 60px) !important; 
                                                                       z-index: 0;}
                                                                 #tPanel {opacity: 0.88;
                                                                          max-height: calc(100vh - 90px);}
                                                                 .btn.collapsed {display: none;}
                                                                 #mobile {display: initial;}
                                                                 #outer {position: relative; padding-bottom: 0px;}
                                                                 #search {width: 275px;}
                                                               }
                                                               @media only screen and (max-width: 600px) {
                                                                 .info.legend.leaflet-control {display: none;}
                                                                 #map {height: calc(100vh - 115px) !important;
                                                                       position: absolute !important;
                                                                       top: 60px;
                                                                       z-index: 0;}
                                                                 .mapBack {height: calc(100vh);}
                                                                 #aPanel {top: 60px !important; 
                                                                          left: 0px !important; 
                                                                          width: 100% !important;}
                                                                .assetsBack {position: absolute;
                                                                             width: 100%;
                                                                             z-index: -1;
                                                                             left: 0px;
                                                                             top: 55px;}
                                                                 #tPanel {margin-bottom:0px; 
                                                                          padding:0px !important; 
                                                                          overflow-y:scroll !important; 
                                                                          max-height: calc(100vh - 65) !important; 
                                                                          min-height: 55px !important; 
                                                                          padding-left: 10px !important; 
                                                                          padding-right: 10px !important;
                                                                          border: none;
                                                                          width: 100%;
                                                                          opacity: 1 !important;}
                                                                 #search {width: calc(100vw - 85px) !important; margin-left:10px !important;}
                                                                 #outer {margin-top: 5px !important; position: absolute;}
                                                                 .btn.collapsed {display: in-line !important;}
                                                               }"),
                          # Generate Map
                          div(class="mapBack", style='position: absolute;
                                                      background-image: url("loading.png");
                                                      background-repeat: no-repeat;
                                                      background-position: center;
                                                      background-size: contain;
                                                      width: 100%;
                                                      z-index: -1;
                                                      left: 0px;
                                                      top: 55px', 
                              leafletOutput("map")),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '325px', style = "z-index: 1000", id = "aPanel",
          wellPanel(id = "tPanel", style = "overflow-y:auto; min-height: 65px;",
                    HTML('<div id="outer" style="z-index: 9; background-color:#ecf0f1;">'),
                    div(style="display:inline-block;", 
                        textInput("search", 
                                  value = "",
                                  label = NULL, 
                                  placeholder = "Search")),
                    tags$style(style="text/css", chartr0('#tPanel #outer .btn .fa:before { content: "\\f056";  }
                                                         #tPanel #outer .btn.collapsed .fa:before { content: "\\f055";  }')),
                    HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile" stye="display: block;"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                         <div id="mobile" class="collapse" style="margin-top:55px;">'),
                    tags$br(),
                    selectInput("neigh_select",
                                label = "Neighborhood",
                                choices = hood_list,
                                selected = "Central Business District",
                                multiple = FALSE,
                                selectize = TRUE),
                    selectInput("basemap_select",
                                label = "Basemap",
                                choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Code for Pittsburgh` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                                selected = "OpenStreetMap.Mapnik")
                   ), 
          HTML("</div>")
        )
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
}



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {  
  setBookmarkExclude(c("GetScreenWidth", "datatable_rows_all"))
  #URL Bookmark 
  sessionStart <- as.numeric(Sys.time())
  names(sessionStart) <- "sessionStart"
  sessionID <- paste(stri_rand_strings(1, 5), gsub("\\.", "-", sessionStart) , "parcels", sep="-")
  names(sessionID) <- "sessionID"
  # Update page URL
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
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
  # Download CSV
  downloadInput <- reactive({
    report <- hoodInput()
    report <- report@data
    # Report Table Search Filter
    if (!is.null(input$datatable_search) && input$datatable_search != "") {
      report <- report[apply(report, 1, function(row){any(grepl(as.character(input$datatable_search), row, ignore.case = TRUE))}), ]
    }
    
    return(report)
  })
  assessmentsLoad <- reactive({
    delinquent <- jsonlite::fromJSON("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22pin%22%20FROM%22ed0d1550-c300-4114-865c-82dc7c23235b%22")$result$records
    cityown <- jsonlite::fromJSON("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22pin%22%20FROM%20%224ff5eb17-e2ad-4818-97c4-8f91fc6b6396%22")$result$records
    liens <- jsonlite::fromJSON(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22pin%22,SUM(amount)%20as%20amount,%20count(pin)%20as%20lien_num%20FROM%20%2265d0d259-3e58-49d3-bebb-80dc75f61245%22%20WHERE%20%22municipality%22%20LIKE%20%27Pittsburgh%27%20AND%20%22satisfied%22=%27False%27%20GROUP%20BY%20%22pin%22"))$result$records
    
    abatement <- jsonlite::fromJSON("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%22pin%22,%22program_name%22,%22start_year%22,%22num_years%22,%22abatement_amt%22%20FROM%22fd924520-d568-4da2-967c-60b3a305e681%22")$result$records %>%
      mutate(tool = paste0("<dt>", program_name, ":", num_years, "</dt>", "<dd>", start_year, ":", abatement_amt, "</dd>")) %>%
      group_by(pin) %>%
      summarise(tt = paste0(tool, collapse = ""))
    
    assessments <-jsonlite::fromJSON("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22PARID%22%2C%22PROPERTYHOUSENUM%22%2C%22PROPERTYFRACTION%22%2C%22PROPERTYADDRESS%22%2C%22PROPERTYZIP%22%2C%22MUNIDESC%22%2C%22TAXDESC%22%2C%22CLASSDESC%22%2C%22OWNERDESC%22%2C%22USEDESC%22%2C%22HOMESTEADFLAG%22%2C%22COUNTYLAND%22%2C%22COUNTYBUILDING%22%2C%22COUNTYTOTAL%22%2C%22SALEPRICE%22%2C%22SALEDATE%22%2C%22YEARBLT%22%20from%20%22518b583f-7cc8-4f60-94d0-174cc98310dc%22%20WHERE%20%22SCHOOLCODE%22%20LIKE%20%2747%27")$result$records %>%
      mutate(delq = PARID %in% delinquent$pin,
             cityown = PARID %in% cityown$pin,
             ADDRESS = paste(PROPERTYHOUSENUM, PROPERTYADDRESS, "PITTSBURGH, PA", PROPERTYZIP)) %>%
      left_join(abatement, by = c("PARID" = "pin")) %>%
      left_join(liens, by = c("PARID" = "pin")) %>%
      select(-c(PROPERTYHOUSENUM, PROPERTYADDRESS, PROPERTYZIP))

    return(assessments)
  })
  # Load Hood Parcel
  hoodLoad <- reactive({
    showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading ', input$neigh_select, '<center></div></font>')), type = "message", id = "hoodMessage", duration = NULL, closeButton = FALSE)
    
    hoodname <- gsub("\\-", "_", input$neigh_select)
    hoodname <- gsub(" ", "_", hoodname)
    hoodname <- gsub("\\.", "", hoodname)
    hoodname <- tolower(hoodname)
    hoodname <- ifelse(hoodname == "", "central_business_district", hoodname)
    
    r <- RETRY("GET", paste0("http://tools.wprdc.org/geoservice/parcels_in/pittsburgh_neighborhood/" , hoodname, "/"), timeout(60)) 
    f <- content(r, "text", encoding = "ISO-8859-1")
    #read content of API call as geojson file
    hood_parcel <- readOGR(f, "OGRGeoJSON", verbose = F)
    assessments <- assessmentsLoad()
    
    hood_parcel <- subset(hood_parcel, select = c(pin, mapblocklo))
    hood_parcel <- merge(hood_parcel, assessments, by.x = "pin", by.y = "PARID", all.x = TRUE)
  
    return(hood_parcel)
  })
  # Load Hood Parcel
  hoodInput <- reactive({
    hood_parcel <- hoodLoad()
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      hood_parcel <- hood_parcel[apply(hood_parcel@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(hood_parcel)
  })
  easterEgg <- reactive({
    if (Sys.Date() == eDay | Sys.Date() == pDay | input$search == "Vote!") {
      month <- as.numeric(format(Sys.Date(), "%m")) 
      
      if (month >= 10) {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*november*"
      } else if (month > 3 ) {
        yearQ <- as.character(as.numeric(format(Sys.Date(), "*%Y")) - 1)
        monthQ <- "*november*"
      } else {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*may"
      }
      
      ids <- getIds("Allegheny County Polling Place Locations") %>%
        filter(grepl(yearQ, name.x, ignore.case = T) & grepl(monthQ, name.x, ignore.case = T))
      
      id <- ids$id.y[1]
      
      load.egg <- ckanSQL(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%22MuniName%22%20=%20%27PITTSBURGH%27")) %>%
        mutate(icon = "election",
               X = as.numeric(X),
               Y = as.numeric(Y),
               tt = paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                           <br><br><b>Location: </b>", LocName,
                           "<br><b>Ward: </b>", Ward,
                           "<br><b>District: </b>", District,
                           "<br><b>Address: </b>", NewAddress,
                           '<br><center><a href="https://alleghenycounty.civicengine.com/" target="_blank">Find your polling place!</a></center>'
               ))
    } else if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
      # Egg
      X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
      Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
      title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "halloween"
      load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
    } else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
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
      load.pools <- readOGR("https://data.wprdc.org/dataset/f7067c4e-0c1e-420c-8c31-f62769fcd29a/resource/77288f26-54a1-4c0c-bc59-7873b1109e76/download/poolsimg.geojson")
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
    return(load.egg)
  })
  output$map <- renderLeaflet({ 
    leaflet()  %>% 
      setView(-79.9959, 40.4406, zoom = 12) %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addLegend(position = "bottomright", colors = c("#ccc7c7", "#4daf4a",  "#ffff33", "#e41a1c") , labels = c("Normal", "Abated Property", "City Property", "Tax Delinquent"), title = "Parcel Info", opacity = .8)
      
  })
  observe({
    # Code for Pittsburgh Basemap
    if (input$basemap_select == "mapStack") {
      leafletProxy("map", session = session) %>%
        clearTiles() %>%
        addTiles(urlTemplate = "http://{s}.sm.mapstack.stamen.com/((terrain-background,$000[@30],$fff[hsl-saturation@80],$1b334b[hsl-color],mapbox-water[destination-in]),(watercolor,$fff[difference],$000000[hsl-color],mapbox-water[destination-out]),(terrain-background,$000[@40],$000000[hsl-color],mapbox-water[destination-out])[screen@60],(streets-and-labels,$fedd9a[hsl-color])[@50])/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="https://cartodb.com/attributions">CARTO</a>', options = providerTileOptions(noWrap = TRUE))
    } else {
      leafletProxy("map", session = session) %>%
        clearTiles() %>%
        addProviderTiles(input$basemap_select, options = providerTileOptions(noWrap = TRUE))
    }
  })
  observeEvent(hoodInput(), {
    hood_parcel <- hoodInput()
    
    #Write inputs to Couch
    if (url.exists(paste0(couchdb_url, ":5984/_utils/"))){
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      inputs <- isolate(reactiveValuesToList(input))
      couchDB$dataList <- c(inputs, sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
    
    if(nrow(hood_parcel@data) > 0) {
      # Trim hood data
      hood_parcel@data <- hood_parcel@data %>% 
        mutate(colorval = case_when(!is.na(tt) ~ "#4daf4a",
                                    cityown == TRUE ~ "#ffff33",
                                    delq == TRUE ~ "#e41a1c",
                                    TRUE ~ "#ccc7c7"),
               popup = paste("<font color='black'><b>Parcel ID:</b>", paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',pin, '" target="_blank">', pin, '</a>'),
                           ifelse(is.na(mapblocklo), "", paste0("<br><b>Lot & Block: </b>", mapblocklo)),
                           ifelse(is.na(ADDRESS), "", paste0("<br><b>Address: </b>", ADDRESS)),
                           ifelse(is.na(MUNIDESC), "", paste0("<br><b>Ward: </b>", MUNIDESC)),
                           ifelse(is.na(OWNERDESC), "", paste0("<br><b>Owner Code: </b>", OWNERDESC)),
                           ifelse(is.na(CLASSDESC), "", paste0("<br><b>Class: </b>", CLASSDESC)),
                           ifelse(is.na(TAXDESC), "", paste0("<br><b>Tax Code: </b>", TAXDESC)),
                           ifelse(is.na(SALEDATE), "", paste0("<br><b>Last Sale Date: </b>", SALEDATE)),
                           ifelse(is.na(SALEPRICE), "", paste0("<br><b>Last Sale Price: </b>", dollarsComma(SALEPRICE))),
                           ifelse(is.na(COUNTYLAND), "", paste0("<br><b>County Land Value: </b>", dollarsComma(COUNTYLAND))),
                           ifelse(is.na(COUNTYBUILDING), "", paste0("<br><b>County Building Value: </b>", dollarsComma(COUNTYBUILDING))),
                           ifelse(is.na(COUNTYTOTAL), "", paste0("<br><b>County Total Value: </b>", dollarsComma(COUNTYTOTAL))),
                           ifelse(is.na(amount), "", paste0("<br><b>Total Lien Amount: </b>", dollarsComma(amount))),
                           ifelse(is.na(lien_num), "", paste0("<br><b>Number of Liens: </b>", lien_num)),
                           hood_parcel$tt,
                           paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=', pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))) %>%
        select(pin, popup, colorval)
      
      leafletProxy("map") %>%
        clearGroup("hood") %>%
        clearGroup("egg") %>%
        flyToBounds(hood_parcel@bbox[1,1], hood_parcel@bbox[2,1], hood_parcel@bbox[1,2], hood_parcel@bbox[2,2]) %>%
        addPolygons(data = hood_parcel, stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000", layerId = ~pin, group = "hood",
                    fill = TRUE, fillColor = ~colorval, fillOpacity = .75,
                    popup = ~popup
          )
   } else {
     print("ran egg")
      if (input$search == "Vote!") {
        egg <- easterEgg()
      } else {
        egg <- easterEgg()
        egg <- egg[sample(1:nrow(egg),1),]
      }
     leafletProxy("map") %>%
       clearGroup("hood") %>%
       clearGroup("egg") %>%
       addMarkers(data = egg, lng= ~X, lat= ~Y, icon = ~icons_egg[icon], popup = ~tt, group = ) %>% 
       setView(-79.9959, 40.4406, zoom = 12)
   }
    removeNotification("hoodMessage")
  }) 
  ##Data Table
  output$datatable <- DT::renderDataTable({
    if (url.exists(paste0(couchdb_url, ":5984/_utils/"))){
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      inputs <- isolate(reactiveValuesToList(input))
      couchDB$dataList <- c(inputs, sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
    hood_parcel <- hoodInput()
    
    hood_parcel@data <- subset(hood_parcel@data, select = c("pin", "mapblocklo", "ADDRESS", "PROPERTYZIP", "MUNIDESC", "TAXDESC",
                                                            "OWNERDESC", "CLASSDESC", "YEARBLT", "SALEDATE", "SALEPRICE",
                                                            "COUNTYLAND", "COUNTYBUILDING", "COUNTYTOTAL", "amount", "lien_num", "delq", "cityown", "tt"))
    colnames(hood_parcel@data) <- c("Parcel ID", "Lot & Block", "Address", "Zip", "Ward", "Tax Code", "Owner Code", "Class", "Year Built",
                                    "Last Sale Date", "Last Sale Price", "County Land Value", "County Building Value", "County Total Value",
                                    "Lien Amount", "Number of Liens","Delinquent", "City Owned", "Abatements")
    hood_parcel@data
  }, escape = FALSE, options = list(scrollX = TRUE), rownames = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$neigh_select, ".csv") },
    content = function(file) {
      write.csv(downloadInput(), file)
    }
  )
  
  })  

enableBookmarking("url")

# Run the application 
shinyApp(ui = ui, server = server) 