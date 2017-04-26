library(shiny)
library(sp)
library(rgeos)
library(maptools)
library(leaflet)
library(rgdal)
library(raster)

#library(gpclib)


#pin_match <- function(x){
# if(parcels$pin = total_parcels$PARCEL_ID){
#  cp_parcel <- merge(total_parcels, parcels, by.x = "PARCEL_ID", by.y = "pin", all.x = TRUE)
#}
#}

#parcels <- pin_match(parcels)

east_end <- readShapeSpatial("east_end_parcels.shp")
west_end <- readShapeSpatial("west_end_parcels.shp")
north_side <- readShapeSpatial("north_side_parcels.shp")
south_hills <- readShapeSpatial("south_hill_parcels.shp")
total_shp <- bind(east_end, north_side, west_end, south_hills)



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  tags$head(tags$title("Burgh's Eye View Parcel Viewer")),
  tags$style(type="text/css", ".container-fluid {padding:0;}"),
  tags$style(type = "text/css", "#map {height: calc(100vh) !important;}"),
  #Set max height for pop-ups
  tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
  leafletOutput("map"),
  absolutePanel(
    #Input panel for Desktops (alpha'd)
    top = 70, left = 50, width = '300px',
    wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: calc(100vh - 85px) !important; overflow: visible;",
              HTML("<br>"),
              selectInput("neigh_select",
                          label = "Neighborhood",
                          choices = levels(total_shp$Neighborho),
                          selected = "Central Business District",
                          multiple = FALSE,
                          selectize = TRUE)
    ), style = "opacity: 0.88"
  )
  
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT",
                       options = providerTileOptions(noWrap = TRUE)
      )
    
    if(!is.null(input$radio == "East End")){ 
      neigh_parcel <- subset(total_shp, Neighborho == input$neigh_select)
      map <- addPolygons(map, data = neigh_parcel,
                         stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         popup = ~(paste("<font color='black'><b>Parcel ID</b>", neigh_parcel$pin,
                                         "<br><b>Owner</b>", neigh_parcel$OWNER,
                                         "<br><b>Address</b>", neigh_parcel$address,
                                         "<br><b>Neighborhood</b>", neigh_parcel$Neighborho,
                                         "<br><b>Ward</b>", neigh_parcel$MUNIDESC,
                                         "<br><b>Owner Description</b>", neigh_parcel$OWNERDESC,
                                         "<br><b>Class Description</b>", neigh_parcel$CLASSDESC,
                                         "<br><b>Use Description</b>", neigh_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date</b>", neigh_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price</b>", neigh_parcel$SALEPRICE,
                                         "<br><b>County Land Value</b>", neigh_parcel$COUNTYLAND,
                                         "<br><b>County Building Value</b>", neigh_parcel$COUNTYBUIL,
                                         "<br><b>County Total Value</b>", neigh_parcel$COUNTYTOTA,
                                         "<br><b>City Taxes</b>", neigh_parcel$City_Tax_D,
                                         "<br><b>School Taxes</b>", neigh_parcel$School_Tax,
                                         "<br><b>Library Taxes</b>", neigh_parcel$Library_Ta, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',neigh_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    else {
      map <- map %>%
        setView(-79.9959, 40.4406, zoom = 10)
    }
    map
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

