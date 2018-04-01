

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
   
   output$mapPlot <- renderPlotly({
     
     if(input$CONTINENT != "All"){
       loans_mapa <- dplyr::filter(inputdata, CONTINENT == input$CONTINENT)
       color_mapa <- dplyr::filter(inputdata, CONTINENT == input$CONTINENT)
       
     }
     
     if(input$REGION != "All"){
       loans_map <- dplyr::filter(loans_mapa, REGION_WB == input$REGION)
       color_map <- dplyr::filter(color_mapa, REGION_WB == input$REGION)
     }
     
     if(input$SUBREGION != "All"){
       loans_map <- dplyr::filter(loans_mapa, SUBREGION == input$SUBREGION)
       color_map <- dplyr::filter(color_mapa, SUBREGION == input$SUBREGION)
       
     }
     if(input$ECONOMY != "All"){
       color_map <- dplyr::filter(color_mapa, ECONOMY == input$ECONOMY)
       
     }
     if(input$INCOME_GRP != "All"){
       color_map <- dplyr::filter(color_mapa, INCOME_GRP == input$INCOME_GRP)
       
     }
     
     
     if(input$METRIC != "mean_term" & input$ACTIVITY == "All"){
      VARIABLE = paste0(input$METRIC, ".plain")
     } else if(input$METRIC != "mean_term" & input$ACTIVITY != "All"){
       VARIABLE = paste0(input$METRIC, ".activity")
     } else if(input$METRIC == "mean_term" & input$ACTIVITY != "All"){
       VARIABLE = "mean_term.activity"
     }
       else (VARIABLE = "mean_term.plain")
     
     if(input$ACTIVITY != "All"){
       color_mapa <- dplyr::filter(color_map, activity == input$ACTIVITY | is.na(activity))
     }
     

      # draw the map
    themap <- ggplot(loans_mapa, aes(x=long, y=lat, group=NAME, text = paste('Name: ', NAME)))+
        theme(panel.background=element_rect(fill="white", color="black"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank())+
        geom_polygon(data= color_mapa, aes(x=long, y=lat, group=group, fill= get(eval(VARIABLE))))+
        geom_path(data = loans_mapa, aes(x=long, y=lat, group=group), color="black", size=0.2)+
        coord_quickmap()+
        labs(title="")
      
    ggplotly(themap, tooltip = c("text"))
   })
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Kiva Loan Data by Country"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("CONTINENT",
                           "CONTINENT:",
                  selected = "Asia", 
                           c("All",  sort(trimws(unique(as.character(inputdata$CONTINENT))))))
      ,       selectInput("REGION",
                          "REGION:",
                          c("All",  sort(trimws(unique(as.character(inputdata$REGION_WB))))))
      ,       selectInput("SUBREGION",
                          "SUBREGION:",
                          c("All",  sort(trimws(unique(as.character(inputdata$SUBREGION))))))
      ,       selectInput("ECONOMY",
                          "ECONOMY:",
                          c("All",  sort(trimws(unique(as.character(inputdata$ECONOMY))))))
      ,       selectInput("INCOME_GRP",
                          "INCOME_GRP:",
                          c("All",  sort(trimws(unique(as.character(inputdata$INCOME_GRP))))))
      ,       selectInput("ACTIVITY",
                          "ACTIVITY:",
                          c("All",  sort(trimws(unique(as.character(inputdata$activity))))))
      
     , selectInput("METRIC",
                  "METRIC:",
                  c("mean_term", "mean_lenders", "mean_amt", "mean_f_term", "mean_f_lenders", "mean_f_amt")
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("mapPlot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

