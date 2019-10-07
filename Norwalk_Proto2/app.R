#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(dplyr)
library(leaflet)
library(tidyr)
library(ggplot2)
#require(rgdal)
library(sf)
library(scales)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Norwalk Transit Difficulty (Made-up Transit Data)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("destination",
                   "Destination",
                   c("Aquarium / Ironworks SONO",
                     "Wall Street Theatre")),
      radioButtons("transit_metric",
                  "Public Transit difficulty",
                  c("Trip time (min)",
                    "Number of transfers",
                    "Walk to first stop (mi.)",
                    "Walk from stop to destination (mi.)",
                    "Total walking distance (mi.)")#,
                  #selected="Median_hh_income"),
      )
      
      
    ),
    
    # Show the map
    mainPanel("In the map below, the color of each area indicates the difficulty of using public transit to get
              from that area to the selected destination. Click the area to view demographic information.",
      leafletOutput("map"),
      "Estimates of transit difficulty come from averaging the difficulty of getting to the destination
      from 5 randomly chosen points within each area at 8am on a Wednesday using Google Maps."
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #turn off scientific notation
  options(scipen=999)
  
  #pull demo_stats for norwalk at the ct level
  load("norwalk_stats.rData")
  #delete row with no input in norwalk stats
  norwalk_stats<-norwalk_stats%>%filter(!is.na(Median_hh_income))
  #create two data frames with some template numbers
  #store them in blank list
  transit_stat_tables=list()
  
  transit_stat_tables[["Aquarium / Ironworks SONO"]]<-data.frame(
    #insert template numbers
    "GEOID"=norwalk_stats$GEOID,
    "Trip time (min)"=runif(22,min=15,max=60),
    "Number of transfers"=as.integer(runif(22,0,3)),
    "Walk to first stop (mi.)"=runif(22,.1,1.5),
    "Walk from stop to destination (mi.)"=runif(22,.1,1.5),
    check.names = FALSE)%>%
    mutate("Total walking distance (mi.)"=`Walk to first stop (mi.)`+
             `Walk from stop to destination (mi.)`)
  #same as above for other destination
  transit_stat_tables[["Wall Street Theatre"]]<-data.frame(
    #insert template numbers
    "GEOID"=norwalk_stats$GEOID,
    "Trip time (min)"=runif(22,min=15,max=60),
    "Number of transfers"=as.integer(runif(22,0,3)),
    "Walk to first stop (mi.)"=runif(22,.1,1.5),
    "Walk from stop to destination (mi.)"=runif(22,.1,1.5),
    check.names = FALSE)%>%
    mutate("Total walking distance (mi.)"=`Walk to first stop (mi.)`+
             `Walk from stop to destination (mi.)`)
  #create a reactive object for destination choice
  chosen_destination<-reactive({
    paste0(input$destination)
  })
  #create a reactive object out of dropdown choice
  chosen_stat<-reactive({
    paste0(input$transit_metric)
  })
  
  chosen_subset<-reactive({
    #start subset at table for chosen destination
    subset<-transit_stat_tables[[chosen_destination()]]%>%
    #limit only to stat selected
      dplyr::rename("chosen_stat"=chosen_stat())
    subset2<-norwalk_stats%>%
      left_join(subset)%>%
      #add a column for the data in the popup
      mutate(pop_up=paste(sep="<br/>",
                          paste0("<b>GEOID:</b> ",GEOID),
                          paste0("<b>Pct Racial Minority: </b>",percent(`Pct Non-White`)),
                          paste0("<b>Pct 65 and older: </b>", percent(`Pct Over 65 y.o.`)),
                          paste0("<b>Pct Under 18: </b>", percent(`Pct Under 18 y.o.`)),
                          paste0("<b>Poverty Rate: </b>", percent(`Poverty Rate`)),
                          paste0("<b>Median Household Income: </b>", dollar(`Median_hh_income`))
                          ))
    subset2
  })

  #create color palette for chosen series
  pal <-
    reactive({
      colorNumeric(palette = "RdYlGn", 
                   domain = chosen_subset()$chosen_stat, n = nrow(norwalk_stats))
    })
  
  
  
  output$map<-renderLeaflet({
    
      #st_transform(crs = "+init=epsg:4326") %>%
      leaflet(width = "50%")%>%
      setView(-73.4167485, 41.101619, 11.25) %>%
      addProviderTiles(provider = "CartoDB.Positron") 
  })
  #create an observer function that adds the layers, so that the view doesn't change every time
  #the user changes the selections
  observe({
    leafletProxy("map")%>%
      clearGroup("poly")%>%
      addPolygons(data=chosen_subset(),popup = ~pop_up,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.4,
                  color = ~ pal()(chosen_stat),
                  #label group for this layer
                  group="poly") 
  })
  observe({
    leafletProxy("map")%>%
      clearControls()%>%
      clearGroup("legend")%>%
      addLegend(data=chosen_subset(),"bottomright", 
                pal = pal(), 
                values = ~ chosen_stat,
                title = "",
                opacity = 1,
                group="legend")
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
