#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Norwalk Demographics and Transit Times"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("demo_choice",
                     "Demographic Statistics",
                     c("Poverty Rate"="Poverty Rate",
                       "Pct Non-White" = "Pct Non-White",
                       "Median Income" = "Median_hh_income"),
                     selected="Median_hh_income")
      ),
      
      # Show the map
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #turn off scientific notation
  options(scipen=999)
  
  #pull demo_stats for norwalk at the ct level
  load("norwalk_stats.RData")
  #delete row with no input in norwalk stats
  norwalk_stats<-norwalk_stats%>%filter(!is.na(Median_hh_income))
  #create a reactive object out of dropdown choice
  chosen_demo<-reactive({
    paste0(input$demo_choice)
  })
  
  chosen_subset<-reactive({
    subset<-norwalk_stats[,c("geometry",chosen_demo())]
    names(subset)[2]<-"chosen_demo"
    subset
  })


  #create color palette for chosen series
  pal <-
    reactive({
      colorNumeric(palette = "viridis", 
                   domain = chosen_subset()$chosen_demo, n = nrow(norwalk_stats))
    })
  
  
  #create sample mouseover pop-up
  sample_popup=data.frame("Travel Est________"=c("Transit Time","First-Mile Dist", "Last-Mile Dist"),
                          "Aquarium___"=c("37 min", ".9 miles", ".3 miles"),
                          "Mall___" =c("43 min", ".5 mile", ".2 miles"),
                          check.names=FALSE)%>%
    kable("html"#, table.attr = 'style = "border-collapse: collapse;"'
    )
  
  
  output$map<-renderLeaflet({
    chosen_subset() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = sample_popup,#paste(sep = "<br/>","Aquarium Mean Transit time:37 minutes",
                  #             "First mile Distance: .4 miles",
                  #             "Last mile Distance: .3 miles"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal()(chosen_demo)) %>%
      addLegend("bottomright", 
                pal = pal(), 
                values = ~ chosen_demo,
                title = "",
                opacity = 1)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

