

library(shiny)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(sp)
library(spdep)
library(raster)
library(RColorBrewer)
library(maptools)
library(classInt)
library(broom)

library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)
library(lubridate)
library(feather)

inputdata <- read_feather("~/Documents/R_Projects/kaggle_kiva/enriched_map_data.feather")

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapPlot <- renderPlot({
     loans_map <- inputdata
     
     if(input$CONTINENT != "All"){
       loans_map <- dplyr::filter(loans_map, CONTINENT == input$CONTINENT)
     }
     
     if(input$METRIC != "mean_term"){
      VARIABLE = input$METRIC
     }
     
      # generate bins based on input$bins from ui.R
     
      # draw the map
      ggplot(loans_map, aes(x=long, y=lat, group=NAME))+
        theme(panel.background=element_rect(fill="white", color="black"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank())+
        geom_polygon(data= loans_map, aes(x=long, y=lat, group=group, fill= get(eval(VARIABLE))))+
        geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
        coord_quickmap()+
        labs(title="")
      
   })
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Loan Data by Country"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("CONTINENT",
                           "CONTINENT:",
                           c("All",  sort(trimws(unique(as.character(inputdata$CONTINENT))))))
      
     , selectInput("METRIC",
                  "METRIC:",
                  c("mean_term" ,   "mean_lenders", "mean_amt")
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

