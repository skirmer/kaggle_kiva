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

source("kiva_eda.R")


# Visual test
ggplot(loans_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group,  fill=loans_map$mean_term))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="World Countries by Loan Term")


ggplot(loans_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group,  fill=loans_map$mean_amt))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="World Countries by Loan Amount")

ggplot(loans_map, aes(x=long, y=lat, group=NAME))+
  facet_grid(CONTINENT~.)+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group,  fill=loans_map$mean_amt))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="Split Countries by Loan Amount")

#### REGIONAL #####

ggplot(asia_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group,  fill=asia_map$mean_amt))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="Asian Countries by Loan Amount")

ggplot(africa_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=africa_map$mean_amt))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="African Countries by Loan Amount")

ggplot(sa_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=sa_map$mean_amt))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="South American Countries by Loan Amount")


### ECONOMY ####

ggplot(sa_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(sa_map$INCOME_GRP)))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="South American Countries by Income")

#ECONOMY    INCOME_GRP  GDP_MD_EST

### GDP ####

ggplot(sa_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=(sa_map$GDP_MD_EST/1000)))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="South American Countries by GDP")


ggplot(asia_map, aes(x=long, y=lat, group=NAME))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())+
  geom_polygon(aes(x=long, y=lat, group=group, fill=(asia_map$GDP_MD_EST/1000)))+
  geom_path(aes(x=long, y=lat, group=group), color="black", size=0.2)+
  coord_quickmap()+
  labs(title="Asian Countries by GDP")

