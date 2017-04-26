##Internal DoF Parcel Map

library(shiny)
library(shinythemes)
library(plyr)
library(sp)
library(rgeos)
library(maptools)
library(leaflet)
library(rgdal)
library(proj4)
library(raster)
library(DT)
library(httr)

# Determine if on mobile device
getWidth <- '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

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
  load.egg <- read.csv("parks.csv")
  load.egg$icon <- "easter_egg"
  load.egg$tt <- "<i>You couldn't find any results, but maybe you can find my eggs.</i>"
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
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5)
)


east_end <- readShapeSpatial("east_end_parcels.shp")
west_end <- readShapeSpatial("west_end_parcels.shp")
north_side <- readShapeSpatial("north_side_parcels.shp")
south_hills <- readShapeSpatial("south_hill_parcels.shp")
hoods_e <- levels(east_end$Neighborho)
hoods_w <- levels(west_end$Neighborho)
hoods_n <- levels(north_side$Neighborho)
hoods_s <- levels(south_hills$Neighborho)
hood_list <- unlist(list(hoods_e, hoods_w, hoods_n, hoods_s))
hood_list <- sort(hood_list)

ui <- shinyUI(navbarPage(id = "navbar",
                         windowTitle = "Burgh's Eye View Parcels",
                         selected = "Parcels",
                         title = HTML('<img src="finance.png" alt="Burghs Eye View" height="85%">'),
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
                          leafletOutput("map"),
                          tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
                          absolutePanel(
                            top = 70, left = 50, width = '300px',
                            wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: calc(100vh - 85px) !important; overflow: visible;",
                                      HTML("<br>"),
                                      selectInput("neigh_select",
                                                  label = "Neighborhood",
                                                  choices = hood_list,
                                                  selected = "Central Business District",
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                                      textInput("search",
                                                label = NULL,
                                                placeholder = "Parcel Search")
                            ), style = "opacity: 0.88"
                          )
                                  ),
                         tabPanel("Data", class = "data", value = "Data",
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
server <- shinyServer(function(input, output) {
  
  
  east_data <- reactive({
    east_data <- east_end
    
    if(nchar(input$search) > 14){
      east_data <- subset(east_data, pin == input$search | CITY_PIN == input$search)
    } else {
      east_data <- subset(east_data, Neighborho == input$neigh_select)
    }
    
    return(east_data)
  })
  
  west_data <- reactive({
    west_data <- west_end
    
    if(nchar(input$search) > 14){
      west_data <- subset(west_data, pin == input$search | CITY_PIN == input$search)
    } else {
      west_data <- subset(west_data, Neighborho == input$neigh_select)
    }
    
    return(west_data)
  })
  
  north_data <- reactive({
    north_data <- north_side
    
    if(nchar(input$search) > 14){
      north_data <- subset(north_data, pin == input$search | CITY_PIN == input$search)
    } else {
      north_data <- subset(north_data, Neighborho == input$neigh_select)
    }
    
    return(north_data)
  })
  
  south_data <- reactive({
    south_data <- south_hills
    
    if(nchar(input$search) > 14){
      south_data <- subset(south_data, pin == input$search | CITY_PIN == input$search)
    } else {
      south_data <- subset(south_data, Neighborho == input$neigh_select)
      
    }
    
    return(south_data)
  })
  

  output$map <- renderLeaflet({
    east_parcel <- east_data()
    west_parcel <- west_data()
    north_parcel <- north_data()
    south_parcel <- south_data()
    
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik",
                       options = providerTileOptions(noWrap = TRUE, minZoom = 1
                       )
      ) %>%
      addLegend("bottomright", colors = c("#4daf4a", "#ffff33", "#e41a1c", "#f781bf", "#984ea3"), labels = c("Abated Properties", "City Owned", "Delinquent at JTS",
                                                                                                             "TIF District", "URA Owned"))
    
    
    if(nrow(east_parcel) > 0){ 
      print(nrow(east_parcel))
      map <- addPolygons(map, data = east_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .75,
                         popup = ~(paste("<font color='black'><b>Parcel ID:</b>", east_parcel$CITY_PIN,
                                         "<br><b>Owner:</b>", east_parcel$OWNER,
                                         "<br><b>Address:</b>", east_parcel$ADDRESS,
                                         "<br><b>Neighborhood:</b>", east_parcel$Neighborho,
                                         "<br><b>Lot & Block:</b>", east_parcel$MAPBLOCKLO,
                                         "<br><b>Council District:</b>", east_parcel$Council_Di,
                                         "<br><b>Ward:</b>", east_parcel$MUNIDESC,
                                         "<br><b>Owner Description:</b>", east_parcel$OWNERDESC,
                                         "<br><b>Class Description:</b>", east_parcel$CLASSDESC,
                                         "<br><b>Use Description:</b>", east_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date:</b>", east_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price:</b>", east_parcel$tt_SALE_PR,
                                         "<br><b>County Land Value:</b>", east_parcel$tt_COUNTYL,
                                         "<br><b>County Building Value:</b>", east_parcel$tt_COUNTYB,
                                         "<br><b>County Total Value:</b>", east_parcel$tt_COUNTYT,
                                         "<br><b>Total Lien Amount:</b>", east_parcel$tt_amount,
                                         "<br><b>Number of Liens:</b>", east_parcel$owedto,
                                         #"<br><b>'17 City Taxes</b>", east_parcel$tt_city_ta,
                                         #"<br><b>'17 School Taxes</b>", east_parcel$tt_school_,
                                         #"<br><b>'17 Library Taxes</b>", east_parcel$tt_lib_tax,
                                         #"<br><b>Current Delinquent Taxes</b>", east_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program:</b>", east_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year:</b>", east_parcel$START_YEAR,
                                         "<br><b>Abatement Period:</b>", east_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", east_parcel$APPROVE_U,
                                         "<br><b>County Identifier:</b>", east_parcel$URL, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',east_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    if(nrow(west_parcel) > 0){ 
      print(nrow(west_parcel))
      map <- addPolygons(map, data = west_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .75,
                         popup = ~(paste("<font color='black'><b>Parcel ID:</b>", west_parcel$CITY_PIN,
                                         "<br><b>Owner:</b>", west_parcel$OWNER,
                                         "<br><b>Address:</b>", west_parcel$ADDRESS,
                                         "<br><b>Neighborhood:</b>", west_parcel$Neighborho,
                                         "<br><b>Lot & Block:</b>", west_parcel$MAPBLOCKLO,
                                         "<br><b>Council District:</b>", west_parcel$Council_Di,
                                         "<br><b>Ward:</b>", west_parcel$MUNIDESC,
                                         "<br><b>Owner Description:</b>", west_parcel$OWNERDESC,
                                         "<br><b>Class Description:</b>", west_parcel$CLASSDESC,
                                         "<br><b>Use Description:</b>", west_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date:</b>", west_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price:</b>", west_parcel$tt_SALE_PR,
                                         "<br><b>County Land Value:</b>", west_parcel$tt_COUNTYL,
                                         "<br><b>County Building Value:</b>", west_parcel$tt_COUNTYB,
                                         "<br><b>County Total Value:</b>", west_parcel$tt_COUNTYT,
                                         "<br><b>Total Lien Amount:</b>", west_parcel$tt_amount,
                                         "<br><b>Number of Liens:</b>", west_parcel$owedto,
                                         #"<br><b>'17 City Taxes</b>", west_parcel$tt_city_ta,
                                         #"<br><b>'17 School Taxes</b>", west_parcel$tt_school_,
                                         #"<br><b>'17 Library Taxes</b>", west_parcel$tt_lib_tax,
                                         #"<br><b>Current Delinquent Taxes</b>", west_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program:</b>", west_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year:</b>", west_parcel$START_YEAR,
                                         "<br><b>Abatement Period:</b>", west_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", west_parcel$APPROVE_U,
                                         "<br><b>County Identifier:</b>", west_parcel$URL, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',west_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    if(nrow(north_parcel) > 0){ 
      print(nrow(north_parcel))
      map <- addPolygons(map, data = north_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .75,
                         popup = ~(paste("<font color='black'><b>Parcel ID:</b>", north_parcel$CITY_PIN,
                                         "<br><b>Owner:</b>", north_parcel$OWNER,
                                         "<br><b>Address:</b>", north_parcel$ADDRESS,
                                         "<br><b>Neighborhood:</b>", north_parcel$Neighborho,
                                         "<br><b>Lot & Block:</b>", north_parcel$MAPBLOCKLO,
                                         "<br><b>Council District:</b>", north_parcel$Council_Di,
                                         "<br><b>Ward:</b>", north_parcel$MUNIDESC,
                                         "<br><b>Owner Description:</b>", north_parcel$OWNERDESC,
                                         "<br><b>Class Description:</b>", north_parcel$CLASSDESC,
                                         "<br><b>Use Description:</b>", north_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date:</b>", north_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price:</b>", north_parcel$tt_SALE_PR,
                                         "<br><b>County Land Value:</b>", north_parcel$tt_COUNTYL,
                                         "<br><b>County Building Value:</b>", north_parcel$tt_COUNTYB,
                                         "<br><b>County Total Value:</b>", north_parcel$tt_COUNTYT,
                                         "<br><b>Total Lien Amount:</b>", north_parcel$tt_amount,
                                         "<br><b>Number of Liens:</b>", north_parcel$owedto,
                                         #"<br><b>'17 City Taxes</b>", north_parcel$tt_city_ta,
                                         #"<br><b>'17 School Taxes</b>", north_parcel$tt_school_,
                                         #"<br><b>'17 Library Taxes</b>", north_parcel$tt_lib_tax,
                                         #"<br><b>Current Delinquent Taxes</b>", north_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program:</b>", north_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year:</b>", north_parcel$START_YEAR,
                                         "<br><b>Abatement Period:</b>", north_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", north_parcel$APPROVE_U,
                                         "<br><b>County Identifier:</b>", north_parcel$URL, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',north_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    } 
    if(nrow(south_parcel) > 0){ 
      print(nrow(south_parcel))
      map <- addPolygons(map, data = south_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .75,
                         popup = ~(paste("<font color='black'><b>Parcel ID:</b>", south_parcel$CITY_PIN,
                                         "<br><b>Owner:</b>", south_parcel$OWNER,
                                         "<br><b>Address:</b>", south_parcel$ADDRESS,
                                         "<br><b>Neighborhood:</b>", south_parcel$Neighborho,
                                         "<br><b>Lot & Block:</b>", south_parcel$MAPBLOCKLO,
                                         "<br><b>Council District:</b>", south_parcel$Council_Di,
                                         "<br><b>Ward:</b>", south_parcel$MUNIDESC,
                                         "<br><b>Owner Description:</b>", south_parcel$OWNERDESC,
                                         "<br><b>Class Description:</b>", south_parcel$CLASSDESC,
                                         "<br><b>Use Description:</b>", south_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date:</b>", south_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price:</b>", south_parcel$tt_SALE_PR,
                                         "<br><b>County Land Value:</b>", south_parcel$tt_COUNTYL,
                                         "<br><b>County Building Value:</b>", south_parcel$tt_COUNTYB,
                                         "<br><b>County Total Value:</b>", south_parcel$tt_COUNTYT,
                                         "<br><b>Total Lien Amount:</b>", south_parcel$tt_amount,
                                         "<br><b>Number of Liens:</b>", south_parcel$owedto,
                                         #"<br><b>'17 City Taxes</b>", south_parcel$tt_city_ta,
                                         #"<br><b>'17 School Taxes</b>", south_parcel$tt_school_,
                                         #"<br><b>'17 Library Taxes</b>", south_parcel$tt_lib_tax,
                                         #"<br><b>Current Delinquent Taxes</b>", south_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program:</b>", south_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year:</b>", south_parcel$START_YEAR,
                                         "<br><b>Abatement Period:</b>", south_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", south_parcel$APPROVE_U,
                                         "<br><b>County Identifier:</b>", south_parcel$URL, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',south_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
##Places easter egg
    if (nrow(south_parcel) + nrow(north_parcel) + nrow(west_parcel) + nrow(east_parcel) == 0) {
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
  
  ##Data Tables
  output$datatable <- DT::renderDataTable({
    df.ee <- as.data.frame(east_end)
    df.we <- as.data.frame(west_end)
    df.ns <- as.data.frame(north_side)
    df.sh <- as.data.frame(south_hills)
    table_data <- rbind(df.ee, df.we, df.ns, df.sh)
    table_data <- subset(table_data, !(is.na(CITY_PIN)))
    table_data <- subset(table_data, select = c("pin", "CITY_PIN", "OWNER", "ADDRESS", "PROP_ZIP", "Neighborho", "MUNIDESC", "TAXDESC", "OWNERDESC", "CLASSDESC", "USEDESC",
                                                "SALEDATE", "SALEPRICE", "COUNTYLAND", "COUNTYBUIL", "COUNTYTOTA", "delq", "PROGRAM_NA",
                                                "ABATEMENT_", "START_YEAR", "APPROVED_U", "lien", "owedto", "tif", "URL"))
   colnames(table_data) <- c("COUNTY_PIN", "CITY_PIN", "OWNER", "ADDRESS", "ZIP", "NEIGHBORHOOD", "MUNIDESC", "TAXDESC", "OWNERDESC", "CLASSDESC", "USEDESC",
                             "SALEDATE", "SALEPRICE", "LANDVAL", "BUILDINGVAL", "TOTALVAL", "DELINQUENT", "ABATE_PROG", "ABATE_AMT", "ABATE_START", "APPROVED_USER",
                             "LIENS", "TOTAL_LIENS", "TIF", "URL")
    table_data

    
  }, filter = "top",
  extensions = 'Buttons',
  options = list(pageLength = 6443,
                 dom = "Bfrtip",
                 buttons = c('copy', 'csv', 'excel'),
                 lengthMenu = c(6443, 7000),
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
  
  # observeEvent({
  #   search <- input$datatablet_search_columns
  #   
  #   
  #   # if(is.null(search)){
  #   #   search <- ""
  #   # }
  #   
  #   print(search)
  # })

  

  
})

# Run the application 
shinyApp(ui = ui, server = server)

