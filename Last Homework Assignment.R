#Last Homework Assignment
#Kim Daniels and Aamirah Botha
#Due: 5 February 2020


# Load libraries
library(tidyverse)
library(ggpubr)
library(readr)
library(scales)
library(ggsn)
library(maps)
library(ggplot2)


#Importing Data

load("C:/Intro_R_2020/data/gebco_sa.Rdata")


#Tidy Data
bathy_wide_tidy <- gather(data=bathy_wide, key="lon",
                          value="elevation",-lat)


#Converting Data to Numeric as Dataframe

#bathy_wide_2<- as.numeric(bathy_wide_tidy)

#bathy_wide_2<- sapply(bathy_wide_converted, as.numeric)

bathy_wide_2 <- as.data.frame(apply(bathy_wide_tidy, 2, as.numeric))


#View Data
head (bathy_wide_2)


#Mapping
ggplot(data = bathy_wide_2, aes(x = lon, y = lat)) + #Data and axes
geom_raster(aes(fill = elevation))+ #Adding elevation values to the map
  scale_fill_gradientn("Elevation (m)",values=scales::rescale(c(-6129,0,1,3374)),
                    colors=c("navyblue", "cyan1", "darkgreen","darkolivegreen",  "burlywood1"))+ #Adding colour and scale
  scale_x_continuous(breaks= seq(10,35,5),
                     labels = c("10", "15", "20", "25" , "30", "35"),
                     position= "bottom")+ #Longitude scale
  scale_y_continuous(expand= c(0,0))+ #Latitude Scale
theme(panel.grid.major= element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank())+ #Crops grid to map
  scale_fill_manual(elevation(c(1000)),color="darkolivegreen") #Attempts to add elevation of 1000 in colour dark olive green

####



