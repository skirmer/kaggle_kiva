

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
library(ggrepel)
library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)
library(lubridate)
library(feather)

inputdata <- read_feather("~/Documents/R_Projects/kaggle_kiva/enriched_map_data.feather")
country_names <- read_feather("~/Documents/R_Projects/kaggle_kiva/country_names.feather")


#listing features
features <- list("Mean Term of Loan (Months)" = "mean_term"
     , "Mean Loan Amount (Local Currency)" = "mean_amt"
     , "Mean Number of Lenders per Loan" = "mean_lenders"
     , "Mean Term of Loan (Months), Female Recipient" = "mean_f_term"
     , "Mean Loan Amount (Local Currency), Female Recipient" = "mean_f_amt"
     , "Mean Number of Lenders per Loan, Female Recipient" = "mean_f_lenders"
     , "Male:Female Ratio of Mean Loan Amount" = "gender_ratio_mean_amt")

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapPlot <- renderPlot({
     loans_map <- inputdata
     color_map <- inputdata
     country_names <- country_names
     features <- features
     
     if(input$CONTINENT != "All"){
       loans_map <- dplyr::filter(loans_map, CONTINENT == input$CONTINENT)
       color_map <- dplyr::filter(color_map, CONTINENT == input$CONTINENT) 
       country_names <- dplyr::filter(country_names, CONTINENT == input$CONTINENT) 
     } else { 
       loans_map <- loans_map
       color_map <- color_map
       country_names <- dplyr::filter(country_names, country %in% sample(country_names$country, 0))
       }
     
     if(input$ECONOMY != "All"){
       color_map <- dplyr::filter(color_map, ECONOMY == input$ECONOMY|is.na(input$ECONOMY))
       
     }
     
     if(input$INCOME_GRP != "All"){
       color_map <- dplyr::filter(color_map, INCOME_GRP == input$INCOME_GRP|is.na(input$INCOME_GRP))
     }
     
     if(input$METRIC != "Mean Term of Loan (Months)"){
      VARIABLE = features[[input$METRIC]]
     } else {VARIABLE = "mean_term"}
     
      # generate bins based on input$bins from ui.R
     
      # draw the map
      ggplot(loans_map, aes(x=long, y=lat, group=country))+
        theme(panel.background=element_rect(fill="white", color="black"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank())+
        scale_fill_gradient(name=input$METRIC)+
        geom_polygon(data=color_map, aes(x=long, y=lat, group=group, fill= get(eval(VARIABLE))))+
        geom_text_repel(data=country_names, aes(long, lat, label = country), size=2)+
        geom_path(data=loans_map, aes(x=long, y=lat, group=group), color="black", size=0.2)+
        coord_quickmap()+
        labs(title=paste0(input$CONTINENT, ": Country Level Analysis of ", input$METRIC))
      
   })
   

}


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme('lumen'),
  # Application title
  titlePanel("Kiva Loan Data by Country"),

  "Welcome! To start exploring the data, select a continent and begin to drill down."  ,br(),
  "Source of shapefiles and general national economic data:", a("Natural Earth Data", 
      href= "http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/"), br(),
  "Source of Kiva data:" , a("Kaggle.com", 
                             href= "https://www.kaggle.com/kiva/data-science-for-good-kiva-crowdfunding/") 
  ,  br(),br(),
  # 
  # 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("CONTINENT",
                           "Continent:",
                  selected = "Asia"
                  ,         c("All",  sort(trimws(unique(as.character(inputdata$CONTINENT))))))
    
      , selectInput("ECONOMY",
                          "Economic Classification:",
                          c("All",  sort(trimws(unique(as.character(inputdata$ECONOMY))))))
      
      , selectInput("INCOME_GRP",
                          "National Income Classification:",
                          c("All",  sort(trimws(unique(as.character(inputdata$INCOME_GRP))))))
      
     , selectInput("METRIC",
                  "Metric:"
                  , selected = "Mean Term of Loan (Months)"
                  , c("Mean Term of Loan (Months)"
                      , "Mean Loan Amount (Local Currency)"
                      , "Mean Number of Lenders per Loan"
                      , "Mean Term of Loan (Months), Female Recipient"
                      , "Mean Loan Amount (Local Currency), Female Recipient"
                      , "Mean Number of Lenders per Loan, Female Recipient"
                      , "Male:Female Ratio of Mean Loan Amount")
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

