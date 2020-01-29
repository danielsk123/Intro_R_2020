#Day 2
#Chapter 6 Exercise
#Kim Daniels
#29_01_2020


#Loading Packages
library(tidyverse)
library(ggpubr)
library(readr)

#Loading Dataset

ChickWeight <- datasets::ChickWeight 

line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(size=2, shape=3, colour="black",fill="white") + #Change the shape, colour, fill and size of each of the points.
  geom_line(aes(group = Chick),size=1) + #See if you can change the thickness of the points/lines.
  labs(x = "Days", y = "Mass (g)")+ 
  theme(legend.text = "Diet Option", legend.title="Diet") #Can you find a way to change the name of the legend? What about its labels?

line_1 #view the plot

box_1 <- ggplot(data = ChickWeight, aes(x = Diet, y = weight)) + #Explore the different geom functions available. These include geom_boxplot, geom_density, etc.
  geom_boxplot(aes(fill = Diet)) +
 
box_1 #view the plot
 
density_1 <-ggplot(ChickWeight,aes(x = Diet, y = weight))+
  geom_density()

density_1 #view the plot

