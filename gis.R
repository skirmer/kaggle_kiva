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
#library(zipcode)
library(broom)

#1. Load shapefile (countries)
shapefile1 <- readOGR(dsn="ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries", verbose=F)

proj4string(shapefile1)

#2. Format for ggplot
shapefile1@data$id <- rownames(shapefile1@data) #create an id based on row number
shapefile1_df<- tidy(shapefile1, region="id") #fortify using the ID so things can be reconnected
shapefile1_df2 <- merge(shapefile1_df, shapefile1, by="id") #get the labels back using the ID as merge connection

#3. Test Run Plot
ggplot(shapefile1_df2, aes(x=long, y=lat, group=NAME_CIAWF))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(shapefile1_df2$CONTINENT)))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="World Countries")

