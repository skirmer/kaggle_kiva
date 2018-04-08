

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

inputdata <- read_feather("enriched_map_data.feather")
country_names <- read_feather("country_names.feather")


#listing features
features <- list("Total Loans" = "total_loans"
     , "Mean Term of Loan (Months)" = "mean_term"
     , "Mean Loan Amount (USD)" = "mean_amt"
     , "Median Loan Amount (USD)" = "median_amt"
     , "Mean Number of Lenders per Loan" = "mean_lenders"
     , "Mean Term of Loan (Months), Female Recipient" = "mean_f_term"
     , "Mean Loan Amount (USD), Female Recipient" = "mean_f_amt"
     , "Median Loan Amount (USD), Female Recipient" = "median_f_amt"
     , "Mean Number of Lenders per Loan, Female Recipient" = "mean_f_lenders"
     , "Male:Female Ratio of Mean Loan Amount" = "gender_ratio_mean_amt"
     , "Male:Female Ratio of Median Loan Amount" = "gender_ratio_median_amt")

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapPlot <- renderPlot(width = 1000, height = 1000, {
     loans_map <- inputdata
     
     country_names <- country_names
     features <- features
     
     if(input$OMIT_SMALL == "True"){
       color_map <- inputdata[inputdata$total_loans > 5,]
     } else {color_map <- inputdata}
     
     if(input$CONTINENT != "All" & input$COUNTRY == "All"){
       loans_map <- dplyr::filter(loans_map, CONTINENT == input$CONTINENT)
       color_map <- dplyr::filter(color_map, CONTINENT == input$CONTINENT) 
       country_names <- dplyr::filter(country_names, CONTINENT == input$CONTINENT) 
     } else if(input$CONTINENT == "All" & input$COUNTRY == "All") { 
       loans_map <- loans_map
       color_map <- color_map
       country_names <- dplyr::filter(country_names, country %in% sample(country_names$country, 0))
     } else if(input$COUNTRY != "All"){
       loans_map <- dplyr::filter(loans_map, country == input$COUNTRY)
       color_map <- dplyr::filter(color_map, country == input$COUNTRY) 
       country_names <- dplyr::filter(country_names, country == input$COUNTRY) 
     } 
     
     if(input$ECONOMY != "All"){
       color_map <- dplyr::filter(color_map, ECONOMY == input$ECONOMY|is.na(input$ECONOMY))
       
     }
     
     if(input$INCOME_GRP != "All"){
       color_map <- dplyr::filter(color_map, INCOME_GRP == input$INCOME_GRP|is.na(input$INCOME_GRP))
     }
     
     if(input$METRIC != "Total Loans" ){
      VARIABLE = features[[input$METRIC]]
     } else {VARIABLE = "total_loans"}
     
      # generate bins based on input$bins from ui.R
     
      # draw the map
      plot1 <- ggplot(loans_map, aes(x=long, y=lat, group=country))+
        theme(panel.background=element_rect(fill="white", color="black"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              legend.position="bottom",
              legend.key.width= unit(5, "cm")
              , legend.text = element_text(size = 14)
              , legend.title = element_text(size = 14)
              , plot.title = element_text(size = 18))+
        scale_fill_gradient(name=input$METRIC)+
        geom_polygon(data=color_map, aes(x=long, y=lat, group=group, fill= get(eval(VARIABLE))))+
        geom_path(data=loans_map, aes(x=long, y=lat, group=group), color="black", size=0.2)+
        geom_label_repel(data=country_names, aes(long, lat, label = country), size=4, color = "black")+
        coord_quickmap()+
        labs(title=paste0(input$CONTINENT, ": Country Level Analysis of ", input$METRIC))
      
      plot1 
   })
   

}


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme('lumen'),
  # Application title
  titlePanel("Kiva Loan Data by Country"),

  "Welcome! This app is still a work in progress, but please feel free to use it. To start exploring the data, select a continent and begin to drill down."  ,br(),
  "Source of shapefiles and general national economic data:", a("Natural Earth Data", 
      href= "http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/"), br(),
  "Source of Kiva data:" , a("Kaggle.com", 
                             href= "https://www.kaggle.com/kiva/data-science-for-good-kiva-crowdfunding/") 
  ,  br(),br(),
  # 
  # 
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3,selectInput("CONTINENT",
                           "Continent:",
                  selected = "Asia"
                  ,         c("All",  sort(trimws(unique(as.character(inputdata$CONTINENT)))))))
    
     ,  column(3,selectInput("COUNTRY",
                             "Country:",
                             selected = "All"
                             ,         c("All",  sort(trimws(unique(as.character(country_names$country)))))))
    
      , column(3,selectInput("ECONOMY",
                          "Economic Classification:",
                          c("All",  sort(trimws(unique(as.character(inputdata$ECONOMY)))))))
      
      , column(3,selectInput("INCOME_GRP",
                          "National Income Classification:",
                          c("All",  sort(trimws(unique(as.character(inputdata$INCOME_GRP)))))))
      
    , column(3,selectInput("OMIT_SMALL",
                           "Omit Countries Below 5 Loans:",
                           c("True",  "False")))
    
     , column(3,selectInput("METRIC",
                  "Metric:"
                  , selected = "Total Loans" 
                  , c(
                    "Total Loans" 
                    , "Mean Term of Loan (Months)"
                      , "Mean Loan Amount (USD)"
                      , "Median Loan Amount (USD)"
                      , "Mean Number of Lenders per Loan"
                      , "Mean Term of Loan (Months), Female Recipient"
                      , "Mean Loan Amount (USD), Female Recipient" 
                      , "Median Loan Amount (USD), Female Recipient"
                      , "Mean Number of Lenders per Loan, Female Recipient"
                      , "Male:Female Ratio of Mean Loan Amount"
                      , "Male:Female Ratio of Median Loan Amount")
    )),
    
    # Show a plot of the generated distribution
    column(12,
      plotOutput("mapPlot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

