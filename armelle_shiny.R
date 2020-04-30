## COVID-19 Food Resource Interactive Tool
## Armelle Coutant, Stanford University (acoutant@stanford.edu), last updated April 2020

## Includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example


# Load required packages

library(shiny)
library(dplyr)
library(leaflet)
library(googlesheets4)
library(knitr)
library(RColorBrewer)
library(highcharter)
library(shinythemes)

#library(tidyverse)
#library(tmaptools)
# library(sf)
# library(htmlwidgets)
# 
# library(censusapi)
# library(mapview)
# library(raster)
# library(rgeos)
# library(tidycensus)
# library(tigris)
# library(usmap)

# library(shiny)
# library(dplyr)
# library(forcats)
# library(leaflet)
# library(ukpolice)
# library(opencage)



# Import and classify data

retailers <- read_sheet("1GvXQYE8OXvE05xRPjywZze6PwDq8gCF5hkfqhMUgZAs",skip=10)
retailers$long <- as.numeric(retailers$long)
retailers$lat <- as.numeric(retailers$lat)

snap <- retailers %>% filter(type == "SNAP_accepting_retailer") 
wic <- retailers %>% filter(type == "WIC_only_store")
snap_wic <- retailers %>% filter(type == "WIC_SNAP_retailer")
snap_restaurant <- retailers %>% filter(type=="SNAP_restaurant")
snap_farmers <- retailers %>% filter(type=="SNAP_farmers_market")

snap_curbside <- snap %>% filter(!is.na(curbside_pickup))
wic_curbside <- wic %>% filter(!is.na(curbside_pickup))
snapwic_curbside <- snap_wic %>% filter(!is.na(curbside_pickup))
snaprest_curbside <- snap_restaurant %>% filter(!is.na(curbside_pickup))
snapfarm_curbside <- snap_farmers %>% filter(!is.na(curbside_pickup))

snap_delivery <- snap %>% filter(!is.na(delivery))
wic_delivery <- wic %>% filter(!is.na(delivery))
snapwic_delivery <- snap_wic %>% filter(!is.na(delivery))
snaprest_delivery <- snap_restaurant %>% filter(!is.na(delivery))
snapfarm_delivery <- snap_farmers %>% filter(!is.na(delivery))

snap_senior <- snap %>% filter(!is.na(senior_hours))
wic_senior <- wic %>% filter(!is.na(senior_hours))
snapwic_senior <- snap_wic %>% filter(!is.na(senior_hours))
snaprest_senior <- snap_restaurant %>% filter(!is.na(senior_hours))
snapfarm_senior <- snap_farmers %>% filter(!is.na(senior_hours))


### BENEFITS LOCATOR FUNCTIONS ###


### INSIGHTS DASHBOARD FUNCTIONS ###


### DATA PROCESSING: X ###


### SHINY UI ###

ui <- bootstrapPage(
    
    navbarPage("COVID-19 FOOD ASSISTANCE", theme = shinytheme("lumen"),
        
            tabPanel("Benefits Locator", icon = icon("map-marker-alt"),

                    sidebarLayout(

                        sidebarPanel(
                            textInput("geocode", "Type an address or location", placeholder = "Example"),
                            checkboxInput("use_location", "Or use your current location?"),
                            actionButton("go", "Search", class = "btn-primary"),
                            highchartOutput("selectstat")
                        ),

                        mainPanel(
                            leafletOutput("map", height = 680)
                        )
                    )
            ),
            
    navbarMenu("Policy Insights", icon = icon("chart-bar"),
               
               tabPanel("US Food Access Measures by County", 
                    
                    div(class="outer", tags$head(includeCSS("styles.css")),
                        
                        leafletOutput("access_map", width="100%", height="100%")
                    
                    )
               ),
               
               tabPanel("Impact of COVID-19 on SNAP Retailer Visits")
               
    ),

            tabPanel("Resource Hub", icon = icon("hands-helping")
                
            ),
    
    navbarMenu("Data", icon = icon("database"),
               
               tabPanel("Food Benefits Data"),
               tabPanel("USDA Food Access Research")
               
    ),
        
            tabPanel("About", icon = icon("info-circle")
                
            )
       
        )

)

# Define server logic
server <- function(input, output) {
    
    output$map <- renderLeaflet({
            
        leaflet() %>% 
            
        addProviderTiles(providers$CartoDB.Positron) %>%
            
        addCircleMarkers(
            data = snap,
            lng = ~long,
            lat =  ~lat, 
            radius = 3,
            color = "#1b9e77",
            stroke = FALSE, fillOpacity = 0.8
        )
    })
    
    output$access_map <- renderLeaflet({
        
        leaflet() %>% 
            
        addProviderTiles(providers$CartoDB.Positron)

    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

