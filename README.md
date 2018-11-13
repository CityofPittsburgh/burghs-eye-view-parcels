# Burghs Eye View - Parcels
<p align="center"><img src="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png"></p>
External Burgh's Eye View displaying City Parcels by Neighborhood.

## Introduction 

### Authors: 
* Maxwell Cercone
* Geoffrey Arnold
* Nicholas Hall
  
### Collaborators:
* Tara Matthews
* Robert Burrack
* Dee Jones
  
### Acknowledgements & Thanks
This application was made possible by a wide number of individuals within and throughout the City of Pittsburgh. First, Laura Meixell for her leadership in seeing this application through to public release and beyond. To Former Chief of Police Cameron McLay for his commitment to Open Data and ensuring the Police are one of our biggest contributors. To Former Chief Innovation Officer for the City of Pittsburgh Debra Lam for her executive sponsorship. To Director Finance Paul Leger for his trasnformative leadership and vision. To our primary funder, the Heinz Endowements for their ongoing support. To all of our friends and family for their compassion and understanding through our late nights and frenzied mornings. To Mayor William Peduto for making the City of Pittsburgh a place where innovative work like this is possible. Finally to the Citizens of Pittsburgh for their generous support of this application since release and their feedback through the 32 Community Meetings our team was able to attend.
  
## Installation & Configuration
Burgh's Eye View is a Shiny application built on a single app file, and therefore only requires R Shiny Server and its dependencies to run. It is hosted through RStudio's shinyapps.io platform but can be run on a local device. The repository as currently configured is ready to deploy minus a few necessary variables supplied by the user.

## Contents
* www: Various images required for the application
* icons: Icons used on the application
  * egg: Icons used for the various season easter eggs on the application.
* about.html: About Page tab html content
* app.R: Main application
* parks.csv: Centroid locations of City parks for easter egg icons.
* hoods.json: List of City neighborhoods
