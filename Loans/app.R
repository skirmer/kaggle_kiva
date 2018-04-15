

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
word_data <- read_feather("word_data.feather")
word_data <- merge(word_data, country_names, by="country")

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



# THE SERVER 
server <- function(input, output) {
   
  
  output$barTable <- DT::renderDataTable({
  
    word_data <- word_data
    
    if(input$CONTINENT != "All" & input$COUNTRY == "All"){
      word_data <- dplyr::filter(word_data, CONTINENT == input$CONTINENT)
      country_names <- dplyr::filter(country_names, CONTINENT == input$CONTINENT) 
      
    } else if(input$CONTINENT == "All" & input$COUNTRY == "All") { 
      word_data <- word_data
      country_names <- dplyr::filter(country_names, country %in% sample(country_names$country, 0))
    } else if(input$COUNTRY != "All"){
      word_data <- dplyr::filter(word_data, country == input$COUNTRY)
      country_names <- dplyr::filter(country_names, country == input$COUNTRY) 
    } 
    
    if(input$ECONOMY != "All"){
      word_data <- dplyr::filter(word_data, ECONOMY == input$ECONOMY|is.na(input$ECONOMY))
    }
    
    if(input$INCOME_GRP != "All"){
      word_data <- dplyr::filter(word_data, INCOME_GRP == input$INCOME_GRP|is.na(input$INCOME_GRP))
    }
    
    if(input$SECTOR != "All"){
      word_data <- dplyr::filter(word_data, sector == input$SECTOR|is.na(input$SECTOR))
    }
    
    if(input$ACTIVITY != "All"){
      word_data <- dplyr::filter(word_data, activity == input$ACTIVITY|is.na(input$ACTIVITY))
    }
    
    word_data2 <- word_data %>%
      group_by(sector, activity, word_short_en) %>%
      summarize(total_count = sum(records)) %>%
      ungroup() %>%
      top_n(25, total_count)
    
    word_data3 <- word_data %>%
      filter(word_short_en %in% word_data2$word_short_en) %>%
      select(continent = CONTINENT, country, sector, activity
             , word_stem = word_short_en, records
             , economy = ECONOMY, income=INCOME_GRP)
    
  })
  
  
  output$barPlot <- renderPlot(width = 1000, height = 600,{
    
    word_data <- word_data
    
    if(input$CONTINENT != "All" & input$COUNTRY == "All"){
      word_data <- dplyr::filter(word_data, CONTINENT == input$CONTINENT)
      country_names <- dplyr::filter(country_names, CONTINENT == input$CONTINENT) 
      
    } else if(input$CONTINENT == "All" & input$COUNTRY == "All") { 
      word_data <- word_data
      country_names <- dplyr::filter(country_names, country %in% sample(country_names$country, 0))
    } else if(input$COUNTRY != "All"){
      word_data <- dplyr::filter(word_data, country == input$COUNTRY)
      country_names <- dplyr::filter(country_names, country == input$COUNTRY) 
    } 
    
    if(input$ECONOMY != "All"){
      word_data <- dplyr::filter(word_data, ECONOMY == input$ECONOMY|is.na(input$ECONOMY))
    }
    
    if(input$INCOME_GRP != "All"){
      word_data <- dplyr::filter(word_data, INCOME_GRP == input$INCOME_GRP|is.na(input$INCOME_GRP))
    }
    
    if(input$SECTOR != "All"){
      word_data <- dplyr::filter(word_data, sector == input$SECTOR|is.na(input$SECTOR))
    }
    
    if(input$ACTIVITY != "All"){
      word_data <- dplyr::filter(word_data, activity == input$ACTIVITY|is.na(input$ACTIVITY))
    }
    
    word_data2 <- word_data %>%
      group_by(sector, activity, word_short_en) %>%
      summarize(total_count = sum(records)) %>%
      ungroup() %>%
      top_n(25, total_count)
    
    
    barplot1 <- ggplot(word_data2) + 
      theme(legend.position = "bottom"
            , legend.text = element_text(size = 14)
            , legend.title = element_text(size = 14)
            , plot.title = element_text(size = 18)
            , axis.text = element_text(size = 15))+
      theme_bw()+
      coord_flip()+
      geom_bar(aes(x=word_short_en, weight=total_count, fill = activity))+
      labs(title = paste0(input$CONTINENT, ": Top 25 Word Stems Used in Loan Request Descriptions")
           , y = "Number of Uses"
           , x = "Stem of Loan Word (Ex. 'provid' for providing, provider)")
  
    barplot1
  })
  
   output$mapPlot <- renderPlot(width = 1000, height = 700, {
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
   
   
   output$mapTable <- DT::renderDataTable({
   
     color_map <- inputdata

     if(input$OMIT_SMALL == "True"){
       color_map <- inputdata[inputdata$total_loans > 5,]
     } else {color_map <- inputdata}
     
     if(input$CONTINENT != "All" & input$COUNTRY == "All"){
       color_map <- dplyr::filter(color_map, CONTINENT == input$CONTINENT) 
     } else if(input$CONTINENT == "All" & input$COUNTRY == "All") { 
       color_map <- color_map
     } else if(input$COUNTRY != "All"){
       color_map <- dplyr::filter(color_map, country == input$COUNTRY) 
     } 
     
     if(input$ECONOMY != "All"){
       color_map <- dplyr::filter(color_map, ECONOMY == input$ECONOMY|is.na(input$ECONOMY))
     }
     
     if(input$INCOME_GRP != "All"){
       color_map <- dplyr::filter(color_map, INCOME_GRP == input$INCOME_GRP|is.na(input$INCOME_GRP))
     }
     
     
     
     
     color_map_table <- color_map %>%
       mutate(mean_term = round(mean_term, 1)
              , mean_lenders = round(mean_lenders, 1)
              , mean_amt = round(mean_amt, 2)
              , median_amt = round(median_amt, 2)
              , mean_f_term = round(mean_f_term, 1)
              , mean_f_lenders = round(mean_f_lenders, 1)
              , mean_f_amt = round(mean_f_amt, 2)
              , median_f_amt = round(median_f_amt, 2)
              , gender_ratio_mean_amt = round(gender_ratio_mean_amt, 2)
              , gender_ratio_median_amt = round(gender_ratio_median_amt, 2)) %>%
       select(continent = CONTINENT, country, economy = ECONOMY
              , income = INCOME_GRP, total_loans, mean_term
              ,mean_lenders           
              , mean_amt, median_amt, mean_f_term, mean_f_lenders, mean_f_amt
              , median_f_amt, gender_ratio_mean_amt, gender_ratio_median_amt) %>%
       unique()
     
     
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
    ,  column(3,selectInput("ECONOMY",
                            "Economic Classification:",
                            c("All",  sort(trimws(unique(as.character(inputdata$ECONOMY)))))))
    
    , column(3,selectInput("INCOME_GRP",
                           "National Income Classification:",
                           c("All",  sort(trimws(unique(as.character(inputdata$INCOME_GRP)))))))
    ),
      tabsetPanel(
      tabPanel("Loan Map", 
               column(3,selectInput("OMIT_SMALL",
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
               plotOutput("mapPlot")
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               ,br()
               , br()
               , br()
               , br()
               , 
               fluidRow(
                 dataTableOutput("mapTable")
               )), 
      tabPanel("Loan Uses", 
               column(3,selectInput("SECTOR",
                                    "Loan Sector:",
                                    c("All",  sort(trimws(unique(as.character(word_data$sector)))))))
               , column(3,selectInput("ACTIVITY",
                                      "Activity Subtype:",
                                      c("All",  sort(trimws(unique(as.character(word_data$activity)))))))
               , 
               plotOutput("barPlot")
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , br()
               , 
               fluidRow(
                dataTableOutput("barTable")
               )))
  )

# Run the application 
shinyApp(ui = ui, server = server)

