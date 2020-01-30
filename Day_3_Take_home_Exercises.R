#Day 3
#Take Home Exercises
#Kim Daniels
#30_01_2020

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

##Exercise 11.4 DIY Maps
##Chapter 10 Exercise
#Cropping World map to Contain only South America
#access the default maps
ggplot() +
  borders() + # The global shape file
  coord_equal()+# Equal sizing for lon/lat 
  coord_equal(xlim = c(-90, -33), ylim = c(-60, 15), expand = 0)+
  scale_fill_manual(values = cols11)+
  labs(title="South America")+
  borders(fill = c("#53E3BE"), colour = "black")


#Reading in Data
library(readr)
ecklonia <- read_csv("data/ecklonia.csv")
View(ecklonia)

# Create a quick scatterplot using own colour gradient
Fig_1 <- ggplot(data = ecklonia, aes(x = primary_blade_length, y = primary_blade_width)) + #x/y= needs to match the column variables
  geom_point(aes(colour = frond_mass))+
  labs(x="Blade length", y= "Blade Width", title="Primary Blade Length and Width", key= "frond mass")+
  scale_colour_gradientn(colours = c("#3A6578","#576987","#776C8F","#976D92",
                                     "#B46E8F","#CE7186"))#adding own colour gradient to frond mass
Fig_1


Fig_2 <- ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter)) + 
  geom_line (aes(colour = primary_blade_length))+
  labs(x="Stipe Length", y= "Stipe Diameter", title="Primary Blade Length")+
  scale_colour_gradientn(colours = c("#53E3BE","#75D790","#98C868",
                                     "#B6B64F","#CEA247","#DC8E51"))
Fig_2

##Exercise 8.5 DIY figures
#Facetting Figures
library(tidyverse)
library(ggpubr)
library(readr)


facetted_graph <-ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter)) +
  geom_line(aes(colour = site))+
  facet_wrap(~site, ncol = 2)+
  labs(x="Stipe Length", y= "Stipe Diameter", title="Stipe Length and Stipe Diameter of Ecklonia maxima at two sites")

facetted_graph


